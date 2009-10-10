#!/bin/bash
clbuild lisp <<EOF
(asdf:operate 'asdf:load-op :qthemlock)
(hi::hemlock)
EOF
