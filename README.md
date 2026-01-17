# RabbitMQ MQTT Username Interceptor

A RabbitMQ plugin that adds the `x-opt-mqtt-username` message annotation to messages published via MQTT.

## Compatibility

This plugin requires RabbitMQ 4.2.0 or later (currently `main` branch only).

The plugin uses the `rabbit_msg_interceptor` behavior which was introduced in RabbitMQ 4.2.

## Installation

Download the `.ez` file from releases and copy to your RabbitMQ plugins directory:

```bash
cp rabbitmq_mqtt_username_interceptor-*.ez $RABBITMQ_HOME/plugins/
rabbitmq-plugins enable rabbitmq_mqtt_username_interceptor
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

When enabled, messages published via MQTT (3.1, 3.1.1, or 5.0) will have an `x-opt-mqtt-username` annotation containing the authenticated username.

- AMQP 0-9-1 consumers see this as a header in `basic.properties`
- AMQP 1.0 consumers see this in `message-annotations`

This is useful for tracking which MQTT client published a message, particularly in multi-tenant environments.

## Configuration

No configuration is required. The plugin activates automatically when enabled.

## License

[Mozilla Public License 2.0](LICENSE)
