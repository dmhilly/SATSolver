SRC = src/dpll.scala

all: help
	-[ -e classes ] || mkdir classes
	scalac -d classes ${SRC}

help:
	@echo "This script compiles the file(s) ${SRC}"
	@echo "Compiled classes are stored in the classes/ directory"
	@echo "Run scala -cp classes DPLL tests/... for results"

.PHONY: all help
