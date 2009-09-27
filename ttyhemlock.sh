#!/bin/bash
cleanup() {
    stty icanon echo stop ^S start ^Q intr ^C
}
trap cleanup EXIT

stty -icanon -echo stop ^- start ^- intr ^-

clbuild lisp <<EOF
(asdf:operate 'asdf:load-op :ttyhemlock)
(hi::old-hemlock)
EOF
