#!/usr/bin/env python

import copy
import json
import os
import sys

def printerr(*args):
  [sys.stderr.write(str(s)) for s in args]

def main():
  with open('./data/control.json', 'r') as control_file:
    control_data = json.load(control_file)
    printerr('\n', control_data, '\n')

  with open('./data/input.json', 'r') as input_file:
    input_data = json.load(input_file)
    printerr('\n', input_data, '\n')

  inputs = os.listdir('./store/input')
  printerr('\n', inputs, '\n')

  printerr('\n', "hello worker", '\n')
  output_data = copy.deepcopy(input_data)
  output_data['step-1'] = 'hello'
  store_data = copy.deepcopy(output_data)
  store_data['storage'] = 'true'

  printerr('\n', output_data, '\n')
  with open('./data/output.json', 'w') as output_file:
    json.dump(output_data, output_file)

  printerr('\n', store_data, '\n')
  with open('./store/output/hello.json', 'w') as store_file:
    json.dump(store_data, store_file)

if __name__ == "__main__":
  main()
