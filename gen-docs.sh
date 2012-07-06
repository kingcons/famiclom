#!/bin/sh
sbcl --eval "(ql:quickload '(famiclom sb-introspect cl-api))" \
     --eval "(cl-api:api-gen :famiclom \"docs/famiclom.html\")" \
     --eval "(progn (terpri) (sb-ext:quit))"
