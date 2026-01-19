# RabbitMQ MQTT Annotation Interceptor

A RabbitMQ plugin that adds MQTT connection metadata as message annotations. By default it adds `x-opt-mqtt-username`, with optional support for vhost and connection name.

## Compatibility

This plugin requires RabbitMQ 4.2.0 or later.

The plugin uses the `rabbit_msg_interceptor` behavior which was introduced in RabbitMQ 4.2.

## Installation

Download the `.ez` file from releases and copy to your RabbitMQ plugins directory:

```bash
cp rabbitmq_mqtt_annotation_interceptor-*.ez $RABBITMQ_HOME/plugins/
rabbitmq-plugins enable rabbitmq_mqtt_annotation_interceptor
```

## Building from Source

```bash
make
make test  # run tests
```

To build against a specific RabbitMQ branch or tag:

```bash
make RABBITMQ_VERSION=main  # default
make RABBITMQ_VERSION=v4.2.x  # once released
```

## How It Works

When enabled, messages published via MQTT (3.1, 3.1.1, or 5.0) will have annotations added based on configuration.

Available annotations:

| Annotation | Config Key | Default | Description |
|------------|------------|---------|-------------|
| `x-opt-mqtt-username` | `username` | `true` | The username used to authenticate the MQTT connection |
| `x-opt-mqtt-vhost` | `vhost` | `false` | The vhost the MQTT connection is connected to |
| `x-opt-mqtt-connection-name` | `connection_name` | `false` | The connection name of the MQTT connection |

- AMQP 0-9-1 consumers see these as headers in `basic.properties`
- AMQP 1.0 consumers see these in `message-annotations`

This is useful for tracking which MQTT client published a message, particularly in multi-tenant environments.

## Configuration

The plugin activates automatically when enabled. By default, only the username annotation is added.

To enable additional annotations, add to `rabbitmq.conf`:

```ini
# Enable vhost annotation
mqtt_annotation_interceptor.vhost = true

# Enable connection name annotation
mqtt_annotation_interceptor.connection_name = true

# Disable username annotation (if you only want vhost/connection_name)
mqtt_annotation_interceptor.username = false
```

## License

[Mozilla Public License 2.0](LICENSE)
