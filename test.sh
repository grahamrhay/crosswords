#!/usr/bin/bash

docker run --name crosswords -it --rm -v ${PWD}:/app -w /app -e REBAR_CACHE_DIR=/app erlang ./rebar3 proper
