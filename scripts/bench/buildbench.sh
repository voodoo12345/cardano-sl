#!/bin/sh

# CONFIG=.. selects section from config file (core/constants.yaml)

stack build --flag cardano-sl-core:-asserts cardano-sl cardano-sl-auxx 
