# `lager_logstash` [![Build Status](https://travis-ci.org/truqu/lager_logstash.svg?branch=develop)](https://travis-ci.org/truqu/lager_logstash)
> Logstash backend for lager

## Configuration

Add `lager` and `lager_logstash` to `rebar.config`:

```erlang
{deps, [lager, lager_logstash]}.
```

Add configuration to `sys.config`:

```erlang
{lager, [{ handlers
         , [{ lager_logstash_backend
            , [ {host, "logstash_host"}
              , {port, 9125}
              ]
            }]
         }]
}
```

> **Note**: Logstash requires timestamps to be in UTC. However, there is no need
> to do anything special to get this working - `lager_logstash` does the
> conversion for you.

Configure a logstash pipeline:

```
input {
  udp {
    codec => "json"
    port  => 9125
  }
}

output {
  ...
}

```

## Supported options

| Option            | Default | Acceptable values                                                                                                                             |
|-------------------|---------|-----------------------------------------------------------------------------------------------------------------------------------------------|
| `host` (required) | /       | `inet:socket_address()` or `inet:hostname()`                                                                                                  |
| `port` (required) | /       | `inet:port_number()`                                                                                                                          |
| `level`           | `info`  | `lager:log_level()` - including [syslog style comparison flags](https://github.com/erlang-lager/lager#syslog-style-loglevel-comparison-flags) |
| `fields`          | `[]`    | `[{atom(), jsx:json_term()}]` - allows specifying a bunch of extra fields to be included                                                      |

## Output format

All metadata is included as fields. Code like this:

```erlang
-module foo

-export([bar/0]).

bar () ->
  lager:info([{foo, bar}], "Hello ~s", ["world"]).
```

Would result in JSON like this:

```javascript
{
  "message": "Hello world",
  "@timestamp": "2019-01-13T21:33:56.925Z",
  "fields": {
    "severity": "info",
    "foo": "bar",
    "application": "otp_application",
    "node": "some_node@localhost",
    "pid": "<0.13919.0>",
    "module": "foo",
    "function": "bar",
    "line": "6"
  }
}
```

## License

Released under the MIT license - please see the `LICENSE` file.

&copy; 2019 - TruQu
