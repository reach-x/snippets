# Makefile examples - GNU Make build automation

# Variables
CC = gcc
CXX = g++
CFLAGS = -Wall -Wextra -O2
CXXFLAGS = $(CFLAGS) -std=c++17
LDFLAGS = -lm
SRC_DIR = src
BUILD_DIR = build
BIN_DIR = bin
TARGET = $(BIN_DIR)/app

# Source files
SOURCES = $(wildcard $(SRC_DIR)/*.c)
OBJECTS = $(SOURCES:$(SRC_DIR)/%.c=$(BUILD_DIR)/%.o)
DEPENDS = $(OBJECTS:.o=.d)

# Default target
all: $(TARGET)

# Link target
$(TARGET): $(OBJECTS) | $(BIN_DIR)
	$(CC) $(OBJECTS) -o $@ $(LDFLAGS)
	@echo "Build complete: $(TARGET)"

# Compile source files
$(BUILD_DIR)/%.o: $(SRC_DIR)/%.c | $(BUILD_DIR)
	$(CC) $(CFLAGS) -MMD -MP -c $< -o $@

# Create directories
$(BUILD_DIR):
	mkdir -p $(BUILD_DIR)

$(BIN_DIR):
	mkdir -p $(BIN_DIR)

# Include dependencies
-include $(DEPENDS)

# Phony targets
.PHONY: all clean install test help

# Clean build artifacts
clean:
	rm -rf $(BUILD_DIR) $(BIN_DIR)
	@echo "Cleaned build artifacts"

# Install target
install: $(TARGET)
	install -m 755 $(TARGET) /usr/local/bin/
	@echo "Installed to /usr/local/bin/"

# Run tests
test: $(TARGET)
	@echo "Running tests..."
	./$(TARGET) --test

# Help target
help:
	@echo "Available targets:"
	@echo "  all      - Build the project (default)"
	@echo "  clean    - Remove build artifacts"
	@echo "  install  - Install the binary"
	@echo "  test     - Run tests"
	@echo "  help     - Show this help message"

# Pattern rules
%.out: %.c
	$(CC) $(CFLAGS) $< -o $@

# Static pattern rule
$(OBJECTS): $(BUILD_DIR)/%.o: $(SRC_DIR)/%.c
	$(CC) $(CFLAGS) -c $< -o $@

# Multiple targets
client server: %: $(SRC_DIR)/%.c
	$(CC) $(CFLAGS) $< -o $(BIN_DIR)/$@

# Conditional compilation
ifeq ($(DEBUG),1)
    CFLAGS += -g -DDEBUG
    $(info Debug mode enabled)
else
    CFLAGS += -DNDEBUG
endif

ifdef VERBOSE
    Q =
else
    Q = @
endif

# Automatic variables examples
example:
	@echo "Target: $@"
	@echo "First prerequisite: $<"
	@echo "All prerequisites: $^"
	@echo "Changed prerequisites: $?"

# Functions
sources := $(wildcard $(SRC_DIR)/*.c)
headers := $(wildcard $(SRC_DIR)/*.h)
objects := $(patsubst $(SRC_DIR)/%.c,$(BUILD_DIR)/%.o,$(sources))
dirs := $(sort $(dir $(objects)))

# String manipulation
uppercase = $(shell echo $(1) | tr '[:lower:]' '[:upper:]')
project_name = myproject
PROJECT_NAME = $(call uppercase,$(project_name))

# File operations
list_sources:
	@echo "Source files:"
	@echo "$(sources)" | tr ' ' '\n'

# Recursion
subdirs = lib tests examples
.PHONY: $(subdirs)

$(subdirs):
	$(MAKE) -C $@

recursive_build: $(subdirs)

# Archive creation
lib/libmylib.a: $(OBJECTS)
	ar rcs $@ $^
	ranlib $@

# Implicit rules (built-in)
# %.o: %.c
#     $(CC) $(CFLAGS) -c $< -o $@

# Disabling implicit rules for specific targets
%.txt: %.md
	pandoc $< -o $@

# Order-only prerequisites (after |)
$(OBJECTS): | $(BUILD_DIR)

# Multiple rules for same target
$(TARGET): CFLAGS += -DVERSION=\"1.0.0\"

# Target-specific variables
debug: CFLAGS += -g -DDEBUG
debug: $(TARGET)

release: CFLAGS += -O3 -DNDEBUG
release: $(TARGET)

# Double-colon rules (allows multiple independent rules)
all:: build
all:: test

# Secondary targets (don't delete intermediate files)
.SECONDARY: $(OBJECTS)

# Precious targets (don't delete on interrupt)
.PRECIOUS: %.o

# Suffixes (old style, prefer pattern rules)
.SUFFIXES:
.SUFFIXES: .c .o

.c.o:
	$(CC) $(CFLAGS) -c $<

# Export variables to sub-makes
export CC CFLAGS

# Include other makefiles
-include config.mk
-include $(wildcard $(BUILD_DIR)/*.d)

# Silent target (suppress echo)
.SILENT: clean

# Print variables for debugging
print-%:
	@echo '$*=$($*)'
	@echo '  origin = $(origin $*)'
	@echo '  flavor = $(flavor $*)'
	@echo '  value  = $($*)'
