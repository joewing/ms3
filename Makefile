
SELECT_PROG=$(if $(shell which $(1)),$(1),$(2))

PYTHON=$(call SELECT_PROG,pypy,python)
FLAKES=$(call SELECT_PROG,pyflakes,echo "WARNING: pyflakes not found")
PEP8=$(call SELECT_PROG,pep8,echo "WARNING: pep8 not found")
NOSETESTS=$(call SELECT_PROG,nosetests,echo "WARNING: nosetests not found")
COVER_PACKAGES=benchmarks database memory

all: lint
	@echo "[TEST]"
	@$(MAKE) test

test:
	@echo "[NOSE]"
	@cd tests && nosetests --with-coverage \
		$(foreach p,$(COVER_PACKAGES), --cover-package $p)

lint:
	@echo "[PEP8]"
	@$(PEP8) .
	@$(MAKE) flakes

flakes:
	@echo "[FLAKES]"
	@$(FLAKES) .

clean:
	@echo "[CLEAN]"
	@find . -name "*.py[oc]" -exec rm {} \;
	@find . -name "__pycache__" -exec rm -fr {} \;
	@rm -f .coverage

