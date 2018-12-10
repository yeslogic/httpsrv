httpsrv
=======

httpsrv is an embedded HTTP server for Mercury programs. It lets you write a
Mercury program that exposes an HTTP interface fairly easily.

httpsrv works with both the low-level and high-level C backends, and should
work on any platform supported by libuv (some small changes may be required).

Requirements
------------

libuv and http-parser:

  * <https://github.com/libuv/libuv>
  * <https://github.com/nodejs/http-parser>

Documentation
-------------

The API begins in `src/httpsrv.m`. There is no documentation yet but hopefully
you can figure it out from the sample application in `sample/httpsrv_test.m`.

Please note that the API may change as needs arise.

Building the sample application
-------------------------------

On Linux, first install libuv and http-parser development libraries using
your distribution's package manager.

Run `make` in the sample directory to produce the `httpsrv_test` program.
Start the program, then go to <http://localhost:8000> in a web browser to
see the output. You can also visit URLs containing different paths or query
parameters, e.g. <http://localhost:8000/foo?bar=1>

You can test the static file and multipart/form-data support as well.
Here is a curl command to upload a file:

    curl http://localhost:8000/upload -F upload=@FILENAME

Then to download the file:

    curl http://localhost:8000/static/FILENAME
