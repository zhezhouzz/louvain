#!/bin/bash

ocamlc graph.ml community.ml utils.ml data.ml louvain.ml && ./a.out
