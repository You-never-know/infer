# Copyright (c) Facebook, Inc. and its affiliates.
#
# This source code is licensed under the MIT license found in the
# LICENSE file in the root directory of this source tree.

TESTS_DIR = ../../..

INFER_OPTIONS = --debug-exceptions --fragment-retains-view-only
INFERPRINT_OPTIONS = \
	--issues-tests-fields file,procedure,line_offset,bug_type,bucket,severity,bug_trace,taint_extra,transitive_callees_extra,autofix \
	--issues-tests
SOURCES = $(wildcard *.java)

include $(TESTS_DIR)/javac.make
