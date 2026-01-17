# Build Notes

## Compatibility

This plugin requires RabbitMQ 4.2.0 or later (currently `main` branch only).

The plugin uses the `rabbit_msg_interceptor` behavior which was introduced in RabbitMQ 4.2.

## Build System

Building standalone RabbitMQ 4.x plugins is challenging because:

1. The `rabbit` package has extensive dependencies (rabbitmq_cli, ra, khepri, etc.)
2. The monorepo's build system (`rabbitmq-components.mk`) assumes `DEPS_DIR` points to the shared `deps/` folder
3. When deps are symlinked, the internal builds still try to resolve deps from the wrong location

### Solution: Build within Cloned Monorepo

The standalone Makefile clones the monorepo and builds the plugin from within it:

1. Clones `rabbitmq-server` to `.build/rabbitmq-server/`
2. Creates `deps/rabbitmq_mqtt_username_interceptor/` with symlinks to our `src/` and `test/`
3. Copies `Makefile.plugin` as the plugin's Makefile (uses standard monorepo includes)
4. Runs the build from within the monorepo context

This approach ensures all dependencies resolve correctly.

## Development Workflow

### Building

```bash
make                           # Build against main branch
make RABBITMQ_VERSION=v4.2.x   # Build against specific version (once released)
make clean                     # Clean build artifacts
make distclean                 # Remove everything including monorepo clone
```

### Creating Distribution

```bash
make dist                      # Creates dist/rabbitmq_mqtt_username_interceptor-X.Y.Z.ez
```

### Testing

Tests require the full RabbitMQ testing infrastructure. The easiest approach is to test within the monorepo:

```bash
# Clone monorepo
git clone https://github.com/rabbitmq/rabbitmq-server.git
cd rabbitmq-server

# Copy plugin
cp -r /path/to/rabbitmq-mqtt-username-interceptor/src deps/rabbitmq_mqtt_username_interceptor/
cp -r /path/to/rabbitmq-mqtt-username-interceptor/test deps/rabbitmq_mqtt_username_interceptor/
cp /path/to/rabbitmq-mqtt-username-interceptor/Makefile.plugin deps/rabbitmq_mqtt_username_interceptor/Makefile

# Run tests
make -C deps/rabbitmq_mqtt_username_interceptor ct
```

## Alternative Approaches Considered

### 1. git-subfolder (erlang.mk built-in)
Used `dep_rabbit = git-subfolder <repo> <version> deps/rabbit`

**Result**: Deps were symlinked but rabbit tried to build and fetch its own deps from the wrong location.

### 2. Custom git_rmq-subfolder
Added custom `dep_fetch_git_rmq-subfolder` method.

**Result**: Same issue - rabbit's build includes `../../rabbitmq-components.mk` which sets `DEPS_DIR` relative to the monorepo structure.

### 3. Complete rabbitmq-components.mk (RabbitMQ 3.12.x style)
The older approach defined ALL component dependencies and custom fetch methods.

**Result**: Works for 3.12.x but RabbitMQ 4.x's monorepo structure is different.

## Files

- `Makefile` - Top-level build script that manages monorepo clone
- `Makefile.plugin` - Standard erlang.mk Makefile used inside the monorepo
- `src/` - Plugin source code
- `test/` - Test suites
- `.build/` - Build directory containing cloned monorepo (gitignored)
- `dist/` - Distribution files (gitignored)
