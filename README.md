# fluentd-erlang

Fluentd client in Erlang.

## tl;dr
### Examples

```erlang
> fluentd:send(<<"Label">>, <<"Data">>).       % Blocking call
ok
> fluentd:send_async(<<"Label">>, <<"Data">>). % Non-blocking call
ok
```

Configuration options:
```erlang
{fluentd,
        [ {ip,                 "127.0.0.1"} % Fluentd Agent Ip
        , {port,               24224}       % Fluentd Agent Port
        , {auto_reconnect,     false}       % Auto Reconnect on closed conn
        , {reconnect_interval, 100}         % Reconnect Interval
        , {enabled, true}                   % Enabled/Disabled
        ]}
```

## License
[MIT license](http://en.wikipedia.org/wiki/MIT_License).
