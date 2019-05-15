#!/bin/bash

RUNJS=${RUNJS:-node}
exec $RUNJS "$(dirname $0)/tests.js"
