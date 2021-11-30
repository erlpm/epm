{{=@@ @@=}}
@@name@@
=====

@@desc@@

Build
-----

    $ epm compile

Use
---

Add the plugin to your epm config:

    {plugins, [
        {@@name@@, {git, "https://host/user/@@name@@.git", {tag, "0.1.0"}}}
    ]}.

Then just call your plugin directly in an existing application:


    $ epm @@name@@
    ===> Fetching @@name@@
    ===> Compiling @@name@@
    <Plugin Output>
