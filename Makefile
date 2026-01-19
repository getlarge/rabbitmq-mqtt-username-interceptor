# RabbitMQ MQTT Annotation Interceptor Plugin
#
# This plugin adds MQTT connection metadata as message annotations.
#
# Build approach:
# Since RabbitMQ 4.x plugins must be built within the monorepo context,
# this Makefile clones the monorepo and builds the plugin from there.

PROJECT = rabbitmq_mqtt_annotation_interceptor
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

# Copy our plugin files into the monorepo's deps
# We use cp instead of symlinks because relative symlinks are fragile
# and absolute symlinks don't work in CI (different paths)
$(PLUGIN_DIR): $(MONOREPO)
	@echo "Copying plugin into monorepo..."
	@rm -rf $(PLUGIN_DIR)
	@mkdir -p $(PLUGIN_DIR)
	@cp -r src $(PLUGIN_DIR)/src
	@cp -r test $(PLUGIN_DIR)/test
	@cp -r priv $(PLUGIN_DIR)/priv
	@cp Makefile.plugin $(PLUGIN_DIR)/Makefile

deps: $(PLUGIN_DIR)

compile: deps
	@echo "Building plugin..."
	$(MAKE) -C $(PLUGIN_DIR)

test ct: deps
	@echo "Running tests..."
	cd $(PLUGIN_DIR) && MAKELEVEL=0 $(MAKE) ct

# Build distribution .ez file
# Note: We run 'make dist' in the plugin dir (not just compile) because:
# - 'make compile' only creates ebin/ with .beam files
# - 'make dist' creates plugins/<name>-<version>/ directory structure
# - We then zip that directory into the .ez archive
dist: deps
	@echo "Building plugin distribution..."
	$(MAKE) -C $(PLUGIN_DIR) dist
	@echo "Creating .ez archive..."
	@mkdir -p dist
	@PLUGIN_DIST_DIR=$$(ls -d $(PLUGIN_DIR)/plugins/$(PROJECT)-* 2>/dev/null | head -1); \
		if [ -z "$$PLUGIN_DIST_DIR" ]; then \
			echo "Error: Plugin dist directory not found in $(PLUGIN_DIR)/plugins/"; \
			exit 1; \
		fi; \
		VERSION=$$(basename "$$PLUGIN_DIST_DIR" | sed 's/$(PROJECT)-//'); \
		cd $(PLUGIN_DIR)/plugins && \
		zip -r $(CURDIR)/dist/$(PROJECT)-$$VERSION.ez $(PROJECT)-$$VERSION
	@echo "Distribution created:"
	@ls -la dist/$(PROJECT)-*.ez

clean:
	@if [ -d $(PLUGIN_DIR) ]; then $(MAKE) -C $(PLUGIN_DIR) clean; fi
	@rm -rf ebin

distclean: clean
	@rm -rf $(BUILD_DIR) dist

# Show help
help:
	@echo "RabbitMQ MQTT Annotation Interceptor Plugin"
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
