CC=gcc
CFLAGS=-Isrc/headers -Wall
SRC_DIR=src
BUILD_DIR=build
TESTS_DIR=tests
SOURCES=$(wildcard $(SRC_DIR)/*.c)
OBJECTS=$(patsubst $(SRC_DIR)/%.c,$(BUILD_DIR)/%.o,$(SOURCES))
EXECUTABLE=acc

all: $(BUILD_DIR) $(EXECUTABLE)

$(BUILD_DIR):
	@mkdir -p $(BUILD_DIR)

$(BUILD_DIR)/%.o: $(SRC_DIR)/%.c
	$(CC) $(CFLAGS) -c $< -o $@

$(EXECUTABLE): $(OBJECTS)
	$(CC) $(CFLAGS) -o $@ $^

test: $(EXECUTABLE)
	@for test_file in $(wildcard $(TESTS_DIR)/*.c); do \
		echo Running $$test_file...; \
		./$(EXECUTABLE) $$test_file; \
	done

clean:
	rm -rf $(BUILD_DIR) $(EXECUTABLE)

.PHONY: all clean test $(BUILD_DIR)
