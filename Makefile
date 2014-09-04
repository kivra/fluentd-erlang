PROJECT = fluentd

# Dependecies ##########################################################
DEPS = stdlib2 msgpack

dep_stdlib2 = git git@github.com:kivra/stdlib2.git          master
dep_msgpack = git git@github.com:msgpack/msgpack-erlang.git 0.3.2

# Standard targets #####################################################
include erlang.mk

ifneq ($(wildcard test/),)
ebin/$(PROJECT).app:: $(shell find test -type f -name \*.erl)
    $(if $(strip $?),$(call compile_erl,$?))
endif

eunit: ERLC_OPTS = $(TEST_ERLC_OPTS) # to have -ifdef(TEST) working properly
eunit: clean app
	$(gen_verbose) erl -noshell -pa ebin -eval 'eunit:test({application, $(PROJECT)}, [])' -s init stop

# eof
