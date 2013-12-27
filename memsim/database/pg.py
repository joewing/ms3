
from __future__ import print_function
import json
import sys
import threading
from sqlalchemy import (create_engine, Table, Column, Integer, String,
                        ForeignKey, MetaData, Text, Float, BigInteger,
                        UniqueConstraint)
from sqlalchemy.sql import select, and_, func
from sqlalchemy.exc import ProgrammingError, IntegrityError

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


metadata = MetaData()
models_table = Table(
    'models', metadata,
    Column('id', Integer, primary_key=True),
    Column('model_hash', String(64), nullable=False, unique=True),
    Column('name', Text, nullable=False),
    Column('data', Text, nullable=False),
)
fpga_results_table = Table(
    'fpga_results', metadata,
    Column('name_hash', String(64), primary_key=True),
    Column('name', Text, nullable=False),
    Column('frequency', Float, nullable=False),
    Column('bram_count', BigInteger, nullable=False),
)
cacti_results_table = Table(
    'cacti_results', metadata,
    Column('name_hash', String(64), primary_key=True),
    Column('name', Text, nullable=False),
    Column('area', Float, nullable=False),
    Column('access_time', Float, nullable=False),
    Column('cycle_time', Float, nullable=False),
)
memories_table = Table(
    'memories', metadata,
    Column('id', Integer, primary_key=True),
    Column('name_hash', String(64), nullable=False, unique=True),
    Column('name', Text, nullable=False),
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
)


class PGDatabase(base.Database):
    """PostgreSQL database connector."""

    def __init__(self, url):
        base.Database.__init__(self)
        self.dbname = 'ms3'
        self.engine = None
        self.url = url
        self.cacti_results = dict()
        self.fpga_results = dict()
        self.models = dict()            # model_hash -> (id, state)
        self.memories = dict()          # memory_hash -> id
        self.result_lock = threading.Lock()

    def connect(self):
        """Establish a database connection."""
        connection_str = '/'.join([self.url, self.dbname])
        self.engine = create_engine(connection_str)
        if not self.engine:
            return False
#        metadata.create_all(bind=self.engine)
        return True

    def _execute(self, stmt):
        return self.engine.execute(stmt)

    def load(self, mod):
        _, state = self.load_model(mod)
        return state

    def save(self, mod, state):
        mod_id = self._get_model_id(mod)
        mod_hash = self.get_hash(mod)
        with self.result_lock:
            self.models[mod_hash] = mod_id, state

        stmt = models_table.update().where(
            models_table.c.id == mod_id
        ).values(
            data=json.dumps(state)
        )
        self._execute(stmt)
        return True

    def load_model(self, mod):

        # Check our local cache.
        mod_hash = self.get_hash(mod)
        with self.result_lock:
            if mod_hash in self.models:
                return self.models[mod_hash]

        # Check the database.
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
            with self.result_lock:
                self.models[mod_hash] = ident, state
            return ident, state

        # Insert a new model.
        stmt = models_table.insert().values(
            model_hash=mod_hash,
            name=str(mod),
            data='{}',
        )
        try:
            self._execute(stmt)
        except ProgrammingError as e:
            if e.orig[1] != '23505':
                raise
        except IntegrityError:
            pass
        return self.load_model(mod)

    def _get_model_id(self, mod):
        ident, _ = self.load_model(mod)
        return ident

    def _get_memory_id(self, mem):

        mem_hash = self.get_hash(mem)
        with self.result_lock:
            if mem_hash in self.memories:
                return self.memories[mem_hash]

        stmt = select([memories_table.c.id]).where(
            memories_table.c.name_hash == mem_hash
        )
        row = self._execute(stmt).first()
        if row:
            ident = row['id']
            with self.result_lock:
                self.memories[mem_hash] = ident
            return ident
        try:
            stmt = memories_table.insert().values(
                name_hash=mem_hash,
                name=str(mem),
            )
            self._execute(stmt)
        except ProgrammingError as e:
            if e.orig[1] != '23505':
                raise
        except IntegrityError:
            pass
        return self._get_memory_id(mem)

    def get_result(self, mod, mem):

        # Check the local cache.
        mod_hash = self.get_hash(mod)
        mem_hash = self.get_hash(mem)
        result_hash = mod_hash + mem_hash
        with self.result_lock:
            if mem_hash in self.results:
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
            with self.result_lock:
                self.results[result_hash] = value
            return value
        else:
            return None

    def add_result(self, mod, mem, value, cost):

        # Insert to our local cache.
        mod_hash = self.get_hash(mod)
        mem_hash = self.get_hash(mem)
        result_hash = mod_hash + mem_hash
        with self.result_lock:
            self.results[result_hash] = value

        # Insert to the database.
        mod_id = self._get_model_id(mod)
        mem_id = self._get_memory_id(mem)
        stmt = results_table.insert().values(
            model_id=mod_id,
            memory_id=mem_id,
            value=value,
            cost=cost,
        )
        try:
            self._execute(stmt)
        except ProgrammingError as e:
            if e.orig[1] != '23505':
                raise
        except IntegrityError:
            pass
        return True

    def get_status(self):
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

    def get_best(self, mod):
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
        mod_id = self._get_model_id(mod)
        stmt = select([
            func.count(results_table.c.value)
        ]).where(results_table.c.model_id == mod_id)
        row = self._execute(stmt).first()
        return row[0] if row else 0

    def get_fpga_result(self, name):

        name_hash = self.get_hash(name)
        with self.result_lock:
            if name_hash in self.fpga_results:
                return self.fpga_results[name_hash]

        stmt = select([fpga_results_table.c.frequency,
                       fpga_results_table.c.bram_count]).where(
            fpga_results_table.c.name_hash == name_hash
        )
        row = self._execute(stmt).first()
        if row:
            temp = (row['frequency'], row['bram_count'])
            with self.result_lock:
                self.fpga_results[name_hash] = temp
            return temp
        else:
            return None

    def add_fpga_result(self, name, frequency, bram_count):

        name_hash = self.get_hash(name)
        with self.result_lock:
            self.fpga_results[name_hash] = (frequency, bram_count)

        stmt = fpga_results_table.insert().values(
            name_hash=name_hash,
            name=str(name),
            frequency=frequency,
            bram_count=bram_count,
        )
        try:
            self._execute(stmt)
        except ProgrammingError as e:
            if e.orig[1] != '23505':
                raise
        except IntegrityError:
            pass
        return True

    def get_cacti_result(self, name):

        name_hash = self.get_hash(name)
        with self.result_lock:
            if name_hash in self.cacti_results:
                return self.cacti_results[name_hash]

        stmt = select([cacti_results_table.c.access_time,
                       cacti_results_table.c.cycle_time,
                       cacti_results_table.c.area]).where(
            cacti_results_table.c.name_hash == name_hash
        )
        row = self._execute(stmt).first()
        if row:
            temp = (row['access_time'], row['cycle_time'], row['area'])
            with self.result_lock:
                self.cacti_results[name_hash] = temp
            return temp
        else:
            return None

    def add_cacti_result(self, name, access_time, cycle_time, area):

        name_hash = self.get_hash(name)
        with self.result_lock:
            self.cacti_results[name_hash] = (access_time, cycle_time, area)

        stmt = cacti_results_table.insert().values(
            name_hash=name_hash,
            name=str(name),
            area=area,
            access_time=access_time,
            cycle_time=cycle_time,
        )
        try:
            self._execute(stmt)
        except ProgrammingError as e:
            if e.orig[1] != '23505':
                raise
        except IntegrityError:
            pass
        return True

    def get_states(self):
        stmt = select([models_table.c.id, models_table.c.name])
        for row in self._execute(stmt):
            yield row['id'], row['name']
