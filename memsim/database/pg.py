
from __future__ import print_function
import json
from sqlalchemy import (create_engine, Table, Column, Integer, String,
                        ForeignKey, MetaData, Text, Float, BigInteger)
from sqlalchemy.sql import select, and_
from sqlalchemy.exc import ProgrammingError

from memsim.database import base

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
    Column('model_id', None, ForeignKey('models.id'), nullable=False),
    Column('memory_id', None, ForeignKey('memories.id'), nullable=False),
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
        self.engine = create_engine(connection_str)
        if not self.engine:
            return False
        metadata.create_all(bind=self.engine)
        return True

    def load(self, m):
        base.Database.load(self, m)
        with self.engine.begin() as conn:
            stmt = select([models_table.c.id, models_table.c.data]).where(
                models_table.c.model_hash == self.model_hash
            )
            row = conn.execute(stmt).first()
            if row:
                self.model_id = row['id']
                self.state = json.loads(row['data'])
                return True
            stmt = models_table.insert().values(
                model_hash=self.model_hash,
                name=str(m),
                data=json.dumps(self.state),
            )
            conn.execute(stmt)
        self.load(m)
        return False

    def save(self):
        stmt = models_table.update().where(
            models_table.c.id == self.model_id
        ).values(
            data=json.dumps(self.state)
        )
        with self.engine.begin() as conn:
            conn.execute(stmt)

    def _get_memory_id(self, mem):
        mem_hash = self.get_hash(mem)
        try:
            with self.engine.begin() as conn:
                stmt = select([memories_table.c.id]).where(
                    memories_table.c.name_hash == mem_hash
                )
                row = conn.execute(stmt).first()
                if row:
                    return row['id']
                stmt = memories_table.insert().values(
                    name_hash=mem_hash,
                    name=str(mem),
                )
                conn.execute(stmt)
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
        with self.engine.begin() as conn:
            row = conn.execute(stmt).first()
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
            with self.engine.begin() as conn:
                conn.execute(stmt)
        except ProgrammingError as e:
            if e.orig[1] != '23505':
                raise

    def get_fpga_result(self, name):
        name_hash = self.get_hash(name)
        if name_hash in self.fpga_results:
            return self.fpga_results[name_hash]
        stmt = select([fpga_results_table.c.frequency,
                       fpga_results_table.c.bram_count]).where(
            fpga_results_table.c.name_hash == name_hash
        )
        with self.engine.begin() as conn:
            row = conn.execute(stmt).first()
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
            with self.engine.begin() as conn:
                conn.execute(stmt)
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
        with self.engine.begin() as conn:
            row = conn.execute(stmt).first()
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
            with self.engine.begin() as conn:
                conn.execute(stmt)
        except ProgrammingError as e:
            if e.orig[1] != '23505':
                raise

    def get_states(self):
        stmt = select([models_table.c.data])
        with self.engine.begin() as conn:
            for row in conn.execute(stmt):
                yield json.loads(row['data'])
