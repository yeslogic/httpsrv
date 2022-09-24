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

The API is subject to change as needs arise.

httpsrv itself runs on a single thread since libuv implements a single
threaded event loop. It is expected that users will spawn multiple *native*
threads to handle requests so that handling of a single request will not
block progress of the rest of the program. You will probably want to
implement a thread pool of some sort.

Mercury green threads (i.e. thread.spawn in non-.par grades) are not supported
because we can't multiplex execution of Mercury code with execution of httpsrv.

Building the sample application
-------------------------------

First install libuv and http-parser development libraries using your
preferred package manager. If you are not using a package manager,
you can try the git submodules in the `externals` directory as a starting
point, but they are not updated regularly and you will need to figure out
how to build them and link them with httpsrv.

Run `make` in the sample directory to produce the `httpsrv_test` program.
On Windows (MinGW), please run `make WINDOWS=1`.

Start the sample application, e.g. `./httpsrv_test`,
and go to <http://localhost:8000> in a web browser to see the output.
You can also visit URLs containing different paths or query parameters,
e.g. <http://localhost:8000/foo?bar=1>

You can test the static file and multipart/form-data support as well.
Here is a curl command to upload a file:

    curl http://localhost:8000/upload -F upload=@FILENAME

Then to download the file:

    curl http://localhost:8000/static/FILENAME
