# RabbitMQ MQTT Username Interceptor Plugin
#
# This plugin adds x-opt-mqtt-username annotation to MQTT messages.
#
# Build approach:
# Since RabbitMQ 4.x plugins must be built within the monorepo context,
# this Makefile clones the monorepo and builds the plugin from there.

PROJECT = rabbitmq_mqtt_username_interceptor
# Note: rabbit_msg_interceptor behavior is only available in main branch (will be in 4.2+)
RABBITMQ_VERSION ?= main

# Build directory within the cloned monorepo
BUILD_DIR = .build
MONOREPO = $(BUILD_DIR)/rabbitmq-server
PLUGIN_DIR = $(MONOREPO)/deps/$(PROJECT)

.PHONY: all deps compile test ct clean distclean dist

all: compile

# Clone the monorepo if needed
$(MONOREPO):
	@echo "Cloning RabbitMQ monorepo ($(RABBITMQ_VERSION))..."
	@mkdir -p $(BUILD_DIR)
	@git clone -q --depth 1 --branch $(RABBITMQ_VERSION) \
		https://github.com/rabbitmq/rabbitmq-server.git $(MONOREPO)

# Link our plugin into the monorepo's deps
$(PLUGIN_DIR): $(MONOREPO)
	@echo "Linking plugin into monorepo..."
	@rm -rf $(PLUGIN_DIR)
	@mkdir -p $(PLUGIN_DIR)
	@ln -sf $(CURDIR)/src $(PLUGIN_DIR)/src
	@ln -sf $(CURDIR)/test $(PLUGIN_DIR)/test
	@cp Makefile.plugin $(PLUGIN_DIR)/Makefile

deps: $(PLUGIN_DIR)

compile: deps
	@echo "Building plugin..."
	$(MAKE) -C $(PLUGIN_DIR)

test ct: deps
	@echo "Running tests..."
	cd $(MONOREPO) && $(MAKE) -C deps/$(PROJECT) ct

# Build distribution .ez file
dist: compile
	@echo "Creating distribution..."
	@mkdir -p dist
	@VERSION=$$(ls $(PLUGIN_DIR)/plugins/ | grep $(PROJECT) | sed 's/$(PROJECT)-//'); \
		cd $(PLUGIN_DIR)/plugins && \
		zip -r $(CURDIR)/dist/$(PROJECT)-$$VERSION.ez $(PROJECT)-$$VERSION
	@echo "Distribution created in dist/"
	@ls -la dist/

clean:
	@if [ -d $(PLUGIN_DIR) ]; then $(MAKE) -C $(PLUGIN_DIR) clean; fi
	@rm -rf ebin

distclean: clean
	@rm -rf $(BUILD_DIR) dist

# Show help
help:
	@echo "RabbitMQ MQTT Username Interceptor Plugin"
	@echo ""
	@echo "Targets:"
	@echo "  all       - Build the plugin (default)"
	@echo "  compile   - Compile the plugin"
	@echo "  test/ct   - Run Common Test suites"
	@echo "  dist      - Create distribution .ez file"
	@echo "  clean     - Clean build artifacts"
	@echo "  distclean - Remove all generated files including monorepo clone"
	@echo ""
	@echo "Variables:"
	@echo "  RABBITMQ_VERSION - RabbitMQ version to build against (default: $(RABBITMQ_VERSION))"
	@echo ""
	@echo "Examples:"
	@echo "  make                           # Build against $(RABBITMQ_VERSION)"
	@echo "  make RABBITMQ_VERSION=main     # Build against main branch"
	@echo "  make RABBITMQ_VERSION=v4.0.x   # Build against 4.0.x"
	@echo "  make test                      # Run tests"
	@echo "  make dist                      # Create .ez file for distribution"
