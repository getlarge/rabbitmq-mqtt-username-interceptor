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
2. **Copies** (not symlinks) `src/` and `test/` to `deps/rabbitmq_mqtt_username_interceptor/`
3. Copies `Makefile.plugin` as the plugin's Makefile (uses standard monorepo includes)
4. Runs the build from within the monorepo context

This approach ensures all dependencies resolve correctly.

### CI/Test Considerations

Running tests requires special handling because `rabbitmq-dist.mk` only installs CLI scripts (rabbitmqctl, etc.) when `MAKELEVEL=0`. The wrapper Makefile explicitly sets `MAKELEVEL=0` when running tests to ensure the CLI tools are available.

We use file copies instead of symlinks because:
- Absolute symlinks don't work in CI (different paths)
- Relative symlinks are fragile and hard to maintain

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

```bash
make test                      # Run tests (handles all setup automatically)
```

The test target:
1. Clones the monorepo if needed
2. Copies plugin files into the monorepo
3. Runs tests with `MAKELEVEL=0` to ensure CLI tools are installed

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

### 4. Symlinks to Source Files
Initial attempt used symlinks (`ln -sf $(CURDIR)/src $(PLUGIN_DIR)/src`).

**Result**: Absolute symlinks don't work in CI because paths differ between local dev and CI environments.

### 5. Using make -C with MAKELEVEL > 0
Running `cd $(MONOREPO) && make -C deps/$(PROJECT) ct` from the wrapper.

**Result**: Tests failed because `rabbitmq-dist.mk` only runs `install-cli` when `MAKELEVEL=0`. Fixed by explicitly setting `MAKELEVEL=0` when running tests.

## Files

- `Makefile` - Top-level build script that manages monorepo clone
- `Makefile.plugin` - Standard erlang.mk Makefile used inside the monorepo
- `src/` - Plugin source code
- `test/` - Test suites
- `.build/` - Build directory containing cloned monorepo (gitignored)
- `dist/` - Distribution files (gitignored)
