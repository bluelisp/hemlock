#!/bin/bash
clbuild lisp <<EOF
(asdf:operate 'asdf:load-op :ttyhemlock)
(hi::old-hemlock)
EOF
