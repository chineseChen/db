rebar3_db2file
=====

A rebar plugin

Build
-----

    $ rebar3 compile

Use
---

Add the plugin to your rebar config:

    {plugins, [
        {rebar3_db2file, {git, "https://host/user/rebar3_db2file.git", {tag, "0.1.0"}}}
    ]}.

Then just call your plugin directly in an existing application:

    $ rebar3 rebar3_db2file
    ===> Fetching rebar3_db2file
    ===> Compiling rebar3_db2file
    <Plugin Output>
