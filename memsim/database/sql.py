from __future__ import print_function
import json
import sys
from sqlalchemy import (create_engine, Table, Column, Integer, String,
                        ForeignKey, MetaData, Text, Float, BigInteger,
                        UniqueConstraint, literal)
from sqlalchemy.sql import select, and_, func, exists

from memsim.resultcache import ResultCache
from memsim.database import base

try:
    import psycopg2
    assert(psycopg2)
    print("Using psycopg2", file=sys.stderr)
except ImportError:
    try:
        from psycopg2cffi import compat
        compat.register()
        print("Using psycopg2cffi", file=sys.stderr)
    except ImportError:
        try:
            from psycopg2ct import compat
            compat.register()
            print("Using psycopg2ct", file=sys.stderr)
        except ImportError:
            print("psycopg2 not available", file=sys.stderr)


# Schema
metadata = MetaData()
models_table = Table(
    'models', metadata,
    Column('id', Integer, primary_key=True),
    Column('model_hash', String(64), nullable=False, unique=True),
    Column('name', Text, nullable=False),
    Column('data', Text, nullable=False),
    implicit_returning=False,
)
fpga_results_table = Table(
    'fpga_results', metadata,
    Column('name_hash', String(64), primary_key=True),
    Column('name', Text, nullable=False),
    Column('frequency', Float, nullable=False),
    Column('bram_count', BigInteger, nullable=False),
    implicit_returning=False,
)
cacti_results_table = Table(
    'cacti_results', metadata,
    Column('name_hash', String(64), primary_key=True),
    Column('name', Text, nullable=False),
    Column('area', Float, nullable=False),
    Column('access_time', Float, nullable=False),
    Column('cycle_time', Float, nullable=False),
    implicit_returning=False,
)
memories_table = Table(
    'memories', metadata,
    Column('id', Integer, primary_key=True),
    Column('name_hash', String(64), nullable=False, unique=True),
    Column('name', Text, nullable=False),
    implicit_returning=False,
)
results_table = Table(
    'results', metadata,
    Column('model_id', None, ForeignKey('models.id'),
           nullable=False, index=True),
    Column('memory_id', None, ForeignKey('memories.id'),
           nullable=False, index=True),
    Column('value', BigInteger, nullable=False),
    Column('cost', BigInteger, nullable=False),
    UniqueConstraint('model_id', 'memory_id'),
    implicit_returning=False,
)


class SQLDatabase(base.BaseDatabase):
    """SQL database connector."""

    def __init__(self, url):
        base.BaseDatabase.__init__(self)
        self.engine = None
        self.url = url
        self.results = ResultCache(16384)
        self.cacti_results = ResultCache(512)
        self.fpga_results = ResultCache(1024)
        self.models = ResultCache(32)       # model_hash -> (id, state)
        self.memories = ResultCache(16384)  # memory_hash -> id
        self.send_count = 0

    def connect(self):
        """Establish a database connection."""
        self.engine = create_engine(self.url)
        if not self.engine:
            return False
        try:
            metadata.create_all(bind=self.engine)
        except:
            # This doesn't work with the pg8000 connector.
            pass
        return True

    def _execute(self, stmt):
        """Execute a query."""
        self.send_count += 1
        return self.engine.execute(stmt)

    def load(self, mod):
        """Load state data from the database for the specified model."""
        _, state = self._load_model(mod)
        return state

    def save(self, mod, state):
        """Save state data to the database."""
        mod_id = self._get_model_id(mod)
        mod_hash = self.get_hash(mod)
        self.models[mod_hash] = mod_id, state

        stmt = models_table.update().where(
            models_table.c.id == mod_id
        ).values(
            data=json.dumps(state)
        )
        self._execute(stmt)
        return True

    def _load_model(self, mod):

        # Check our local cache.
        mod_hash = self.get_hash(mod)
        if mod_hash in self.models:
            return self.models[mod_hash]

        # Check the database.
        # Note that this is the model likely senario.
        stmt = select([
            models_table.c.id,
            models_table.c.data
        ]).where(
            models_table.c.model_hash == mod_hash
        )
        row = self._execute(stmt).first()
        if row:
            ident = row['id']
            state = json.loads(row['data'])
            self.models[mod_hash] = ident, state
            return ident, state

        # Insert a new model.
        stmt = models_table.insert().from_select(
            [
                models_table.c.model_hash,
                models_table.c.name,
                models_table.c.data,
            ], select([
                literal(mod_hash),
                literal(str(mod)),
                literal('{}'),
            ]).where(
                ~exists([models_table.c.id]).where(
                    models_table.c.model_hash == mod_hash
                )
            )
        )
        self._execute(stmt)
        return self._load_model(mod)

    def _get_model_id(self, mod):
        ident, _ = self._load_model(mod)
        return ident

    def _get_memory_id(self, mem):

        # Check the local cache.
        mem_hash = self.get_hash(mem)
        if mem_hash in self.memories:
            return self.memories[mem_hash]

        # Attempt to insert a new memory.
        # This is the expected case.
        stmt = memories_table.insert().from_select(
            [
                memories_table.c.name_hash,
                memories_table.c.name,
            ], select([
                literal(mem_hash),
                literal(str(mem)),
            ]).where(
                ~exists([memories_table.c.id]).where(
                    memories_table.c.name_hash == mem_hash
                )
            )
        )
        self._execute(stmt)

        # Check the database.
        stmt = select([memories_table.c.id]).where(
            memories_table.c.name_hash == mem_hash
        )
        row = self._execute(stmt).first()
        ident = row['id']
        self.memories[mem_hash] = ident
        return ident

    def get_result(self, mod, mem):
        """Look up the result for the specified model.

        This will return the result if found.  If not found and the
        first request for the result, it will return None, otherwise
        it will return -1 (indicating that another process is computing
        the result).
        """

        # Check the local cache.
        mod_hash = self.get_hash(mod)
        mem_hash = self.get_hash(mem)
        result_hash = mod_hash + mem_hash
        if result_hash in self.results:
            return self.results[result_hash]

        # Check the database.
        mod_id = self._get_model_id(mod)
        memory_id = self._get_memory_id(mem)
        stmt = select([results_table.c.value]).where(
            and_(
                results_table.c.model_id == mod_id,
                results_table.c.memory_id == memory_id,
            )
        )
        row = self._execute(stmt).first()
        if row:
            value = row['value']
            self.results[result_hash] = value
            return value
        else:
            self.results[result_hash] = -1
            return None

    def add_result(self, mod, mem, value, cost):
        """Add a result for the specified model."""

        assert(value > 0)

        # Insert to our local cache.
        mod_hash = self.get_hash(mod)
        mem_hash = self.get_hash(mem)
        result_hash = mod_hash + mem_hash
        self.results[result_hash] = value

        # Insert to the database.
        mod_id = self._get_model_id(mod)
        mem_id = self._get_memory_id(mem)
        stmt = results_table.insert().from_select(
            [
                results_table.c.model_id,
                results_table.c.memory_id,
                results_table.c.value,
                results_table.c.cost,
            ], select([
                literal(mod_id),
                literal(mem_id),
                literal(value),
                literal(cost),
            ]).where(
                ~exists([results_table.c.model_id]).where(
                    and_(
                        results_table.c.model_id == mod_id,
                        results_table.c.memory_id == mem_id,
                    )
                )
            )
        )
        self._execute(stmt)
        return True

    def get_best(self, mod):
        """Get the best result for the specified model.

        Returns (name, value, cost).
        """
        mod_id = self._get_model_id(mod)
        min_query = select([
            func.min(results_table.c.value)
        ]).where(results_table.c.model_id == mod_id)
        stmt = select([
            memories_table.c.name,
            results_table.c.value,
            results_table.c.cost,
        ]).where(
            and_(
                memories_table.c.id == results_table.c.memory_id,
                results_table.c.model_id == mod_id,
                results_table.c.value == min_query,
            )
        ).order_by(
            results_table.c.cost,
            func.length(memories_table.c.name),
        )
        row = self._execute(stmt).first()
        if row:
            return row['name'], row['value'], row['cost']
        else:
            return None, 0, 0

    def get_result_count(self, mod):
        """Get the total number of results for the specified model."""
        mod_id = self._get_model_id(mod)
        stmt = select([
            func.count(results_table.c.value)
        ]).where(results_table.c.model_id == mod_id)
        row = self._execute(stmt).first()
        return row[0] if row else 0

    def get_fpga_result(self, name):
        """Get an FPGA timing result."""

        # Check the local cache.
        name_hash = self.get_hash(name)
        if name_hash in self.fpga_results:
            return self.fpga_results[name_hash]

        # Check the database.
        stmt = select([
            fpga_results_table.c.frequency,
            fpga_results_table.c.bram_count
        ]).where(
            fpga_results_table.c.name_hash == name_hash
        )
        row = self._execute(stmt).first()
        if row:
            temp = (row['frequency'], row['bram_count'])
            self.fpga_results[name_hash] = temp
            return temp
        else:
            return None

    def add_fpga_result(self, name, frequency, bram_count):
        """Add an FPGA timing result."""

        # Insert into the local cache.
        name_hash = self.get_hash(name)
        self.fpga_results[name_hash] = (frequency, bram_count)

        # Insert into the database.
        stmt = fpga_results_table.insert().from_select(
            [
                fpga_results_table.c.name_hash,
                fpga_results_table.c.name,
                fpga_results_table.c.frequency,
                fpga_results_table.c.bram_count,
            ], select([
                literal(name_hash),
                literal(str(name)),
                literal(frequency),
                literal(bram_count),
            ]).where(
                ~exists([fpga_results_table.c.name_hash]).where(
                    fpga_results_table.c.name_hash == name_hash
                )
            )
        )
        self._execute(stmt)
        return True

    def get_cacti_result(self, name):
        """Get a CACTI result."""

        # Check the local cache.
        name_hash = self.get_hash(name)
        if name_hash in self.cacti_results:
            return self.cacti_results[name_hash]

        # Check the database.
        stmt = select([
            cacti_results_table.c.access_time,
            cacti_results_table.c.cycle_time,
            cacti_results_table.c.area
        ]).where(
            cacti_results_table.c.name_hash == name_hash
        )
        row = self._execute(stmt).first()
        if row:
            temp = (row['access_time'], row['cycle_time'], row['area'])
            self.cacti_results[name_hash] = temp
            return temp
        else:
            return None

    def add_cacti_result(self, name, access_time, cycle_time, area):
        """Add a CACTI result."""

        # Insert into the local cache.
        name_hash = self.get_hash(name)
        self.cacti_results[name_hash] = (access_time, cycle_time, area)

        # Insert into the database.
        stmt = cacti_results_table.insert().from_select(
            [
                cacti_results_table.c.name_hash,
                cacti_results_table.c.name,
                cacti_results_table.c.area,
                cacti_results_table.c.access_time,
                cacti_results_table.c.cycle_time,
            ], select([
                literal(name_hash),
                literal(str(name)),
                literal(area),
                literal(access_time),
                literal(cycle_time),
            ]).where(
                ~exists([cacti_results_table.c.name_hash]).where(
                    cacti_results_table.c.name_hash == name_hash
                )
            )
        )
        self._execute(stmt)
        return True

    def get_status(self):
        """Get the status of all models.

        Returns (name, evaluations, value).
        """
        stmt = select([
            models_table.c.name.label('name'),
            func.min(results_table.c.value).label('value'),
            func.count(results_table.c.value).label('evals'),
        ]).where(
            results_table.c.model_id == models_table.c.id
        ).group_by(
            models_table.c.name
        )
        for row in self._execute(stmt):
            yield row['name'], row['evals'], row['value']

    def get_states(self):
        """Get all model IDs and names from the database."""
        stmt = select([models_table.c.id, models_table.c.name])
        for row in self._execute(stmt):
            yield row['id'], row['name']
