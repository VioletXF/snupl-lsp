

CC=g++
SRC_DIR=src
SNUPLC_SRC_DIR=$(SNUPLC_DIR)/src
OBJ_DIR=obj
SNUPLC_OBJ_DIR=$(SNUPLC_DIR)/obj
DEP_DIR=.deps
SNUPLC_DEP_DIR=$(SNUPLC_DIR)/.deps

CCFLAGS=-std=c++11 -Wall -g -O2 -Iinclude
DEPFLAGS=-MMD -MP -MT $@ -MF $(DEP_DIR)/$*.d

BACKEND=backend.cpp \
	backendAMD64.cpp
BASE=environment.cpp \
	target.cpp \
	$(BACKEND)
SCANNER=scanner.cpp
PARSER=parser.cpp \
	type.cpp \
	symtab.cpp \
	data.cpp \
	ast.cpp ast_semanal.cpp ast_tacgen.cpp \
	ir.cpp
SOURCES=$(BASE) $(PARSER) $(SCANNER)

DEPS=$(SOURCES:%.cpp=$(SNUPLC_DEP_DIR)/%.d)
OBJ_SCANNER=$(patsubst %.cpp,$(SNUPLC_OBJ_DIR)/%.o, $(SCANNER))
OBJ_PARSER=$(patsubst %.cpp,$(SNUPLC_OBJ_DIR)/%.o, $(BASE) $(SCANNER) $(PARSER))

all: snupld
$(OBJ_DIR)/%.o: $(SRC_DIR)/%.cpp | $(DEP_DIR) $(OBJ_DIR)
	$(CC) $(CCFLAGS) $(DEPFLAGS) -c -o $@ $< -I$(SNUPLC_SRC_DIR)

$(DEP_DIR):
	@mkdir -p $(DEP_DIR)

$(OBJ_DIR):
	@mkdir -p $(OBJ_DIR)

# Just skip missing files (for SnuPL2 cross-phase compatibility)
$(SNUPLC_OBJ_DIR)/%.o:
	@echo >> $@

-include $(DEPS)
ifdef SNUPLC_DIR
snuplc:
	$(MAKE) -C $(SNUPLC_DIR) all
snupld: snuplc $(OBJ_DIR)/snupld.o $(OBJ_PARSER)
	$(CC) $(CCFLAGS) -o $@ $(OBJ_DIR)/snupld.o $(OBJ_PARSER)
install: snupld
	cp -f snupld /usr/local/bin/snupld

mrproper: clean
	$(MAKE) -C $(SNUPLC_DIR) mrproper
else
snupld install:
	@echo "Usage: SNUPLC_DIR=/path/to/snuplc make $@ (do not include /src)"
	@exit 1

mrproper: clean
endif


clean:
	rm -rf $(OBJ_DIR) $(DEP_DIR) $(SNUPLC_OBJ_DIR) $(SNUPLC_DEP_DIR) snupld

