#!/bin/bash
cleanup() {
    stty icanon echo
}
trap cleanup EXIT

stty -icanon -echo
clbuild lisp <<EOF
(require :ttyhemlock)
(hi::old-hemlock)
EOF
