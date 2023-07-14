#!/bin/bash

mkdir -p target/progress_projects
cd target/progress_projects

# small util library
git clone https://github.com/pypa/build
# web framework that implements a lot of magic
git clone https://github.com/django/django
# an ML project
git clone https://github.com/huggingface/transformers
# type annotations
git clone https://github.com/python/typeshed
# python 3.11, typing 100% test coverage
git clone https://github.com/pypi/warehouse
# django project
git clone https://github.com/zulip/zulip

for i in ./*/; do (cd $i && echo $i "$(git rev-parse HEAD)"); done
# ./build/ 5800521541e5e749d4429617420d1ef8cdb40b46
# ./django/ 0016a4299569a8f09ff24053ff2b8224f7fa4113
# ./transformers/ 5bb4430edc7df9f9950d412d98bbe505cc4d328b
# ./typeshed/ 57c435cd7e964290005d0df0d9b5daf5bd2cbcb1
# ./warehouse/ e72cca94e7ac0dbe095db5c2942ad9f2f51b30cc
# ./zulip/ 1cd587d24be1d668fcf6d136172bfec69e35cb75

cd ../..

cargo run --bin ruff_dev -- format-dev --multi-project ~/disk/progress_projects/ > target/progress_projects/report.txt
