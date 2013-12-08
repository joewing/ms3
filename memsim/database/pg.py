
from __future__ import print_function
import json
from sqlalchemy import (create_engine, Table, Column, Integer, String,
                        ForeignKey, MetaData, Text, Float, BigInteger)
from sqlalchemy.sql import select, and_, func
from sqlalchemy.exc import ProgrammingError
from sqlalchemy.pool import SingletonThreadPool

from memsim.database import base

try:
    import psycopg2
    assert(psycopg2)
    print("Using psycopg2")
except ImportError:
    try:
        from psycopg2cffi import compat
        compat.register()
        print("Using psycopg2cffi")
    except ImportError:
        try:
            from psycopg2ct import compat
            compat.register()
            print("Using psycopg2ct")
        except ImportError:
            print("psycopg2 not available")


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
)


class PGDatabase(base.Database):
    """PostgreSQL database connector."""

    def __init__(self, url):
        base.Database.__init__(self)
        self.dbname = 'ms3'
        self.engine = None
        self.url = url
        self.model_id = 0
        self.cacti_results = dict()
        self.fpga_results = dict()

    def connect(self):
        """Establish a database connection."""
        connection_str = '/'.join([self.url, self.dbname])
        self.engine = create_engine(connection_str,
                                    pool_size=1,
                                    poolclass=SingletonThreadPool)
        if not self.engine:
            return False
        metadata.create_all(bind=self.engine)
        return True

    def _execute(self, stmt):
        return self.engine.execute(stmt)

    def load(self, m):
        base.Database.load(self, m)
        stmt = select([models_table.c.id, models_table.c.data]).where(
            models_table.c.model_hash == self.model_hash
        )
        row = self._execute(stmt).first()
        if row:
            self.model_id = row['id']
            self.state = json.loads(row['data'])
            return True
        stmt = models_table.insert().values(
            model_hash=self.model_hash,
            name=str(m),
            data=json.dumps(self.state),
        )
        try:
            self._execute(stmt)
        except ProgrammingError as e:
            if e.orig[1] != '23505':
                raise
        self.load(m)
        return False

    def save(self):
        stmt = models_table.update().where(
            models_table.c.id == self.model_id
        ).values(
            data=json.dumps(self.state)
        )
        self._execute(stmt)

    def _get_memory_id(self, mem):
        mem_hash = self.get_hash(mem)
        stmt = select([memories_table.c.id]).where(
            memories_table.c.name_hash == mem_hash
        )
        row = self._execute(stmt).first()
        if row:
            return row['id']
        try:
            stmt = memories_table.insert().values(
                name_hash=mem_hash,
                name=str(mem),
            )
            self._execute(stmt)
        except ProgrammingError as e:
            if e.orig[1] != '23505':
                raise
        return self._get_memory_id(mem)

    def get_result(self, mem):
        mem_hash = self.get_hash(mem)
        if mem_hash in self.results:
            return self.results[mem_hash]
        memory_id = self._get_memory_id(mem)
        stmt = select([results_table.c.value]).where(
            and_(
                results_table.c.model_id == self.model_id,
                results_table.c.memory_id == memory_id,
            )
        )
        row = self._execute(stmt).first()
        return row['value'] if row else None

    def add_result(self, mem, value):
        mem_hash = self.get_hash(mem)
        self.results[mem_hash] = value
        mem_id = self._get_memory_id(mem)
        stmt = results_table.insert().values(
            model_id=self.model_id,
            memory_id=mem_id,
            value=value,
        )
        try:
            self._execute(stmt)
        except ProgrammingError as e:
            if e.orig[1] != '23505':
                raise

    def get_best(self):
        min_query = select([
            func.min(results_table.c.value)
        ]).where(results_table.c.model_id == self.model_id)
        stmt = select([
            memories_table.c.name,
            results_table.c.value,
        ]).where(
            and_(
                memories_table.c.id == results_table.c.memory_id,
                results_table.c.model_id == self.model_id,
                results_table.c.value == min_query,
            )
        )
        best_name = None
        best_value = 0
        for row in self._execute(stmt):
            name = row['name']
            value = row['value']
            if (not best_name) or len(name) < len(best_name):
                best_name = name
                best_value = value
        return best_name, best_value

    def get_fpga_result(self, name):
        name_hash = self.get_hash(name)
        if name_hash in self.fpga_results:
            return self.fpga_results[name_hash]
        stmt = select([fpga_results_table.c.frequency,
                       fpga_results_table.c.bram_count]).where(
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
        name_hash = self.get_hash(name)
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

    def get_cacti_result(self, name):
        name_hash = self.get_hash(name)
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
            self.cacti_results[name_hash] = temp
            return temp
        else:
            return None

    def add_cacti_result(self, name, access_time, cycle_time, area):
        name_hash = self.get_hash(name)
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

    def get_states(self):
        stmt = select([models_table.c.data])
        for row in self._execute(stmt):
            yield json.loads(row['data'])
