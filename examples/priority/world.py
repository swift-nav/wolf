#!/usr/bin/env python

import copy
import json
import os
import sys
import time

def printerr(*args):
  [sys.stderr.write(str(s)) for s in args]

def main():
  printerr('\n', '++++++ START world.py +++++', '\n')

  with open('./data/control.json', 'r') as control_file:
    control_data = json.load(control_file)
#   printerr('\n', control_data, '\n')

  with open('./data/input.json', 'r') as input_file:
    input_data = json.load(input_file)
    printerr('\n', input_data, '\n')

  inputs = os.listdir('./store/input')
# printerr('\n', inputs, '\n')

# printerr('\n', "world worker", '\n')
  output_data = copy.deepcopy(input_data)
  output_data['step-2'] = 'world'
  store_data = copy.deepcopy(output_data)
  store_data['storage'] = 'true'

  printerr('\n', output_data, '\n')
  with open('./data/output.json', 'w') as output_file:
    json.dump(output_data, output_file)

# printerr('\n', store_data, '\n')
  with open('./store/output/world.json', 'w') as store_file:
    json.dump(store_data, store_file)

  time.sleep(3)
  printerr('\n', '++++++ FINISH world.py +++++', '\n')

if __name__ == "__main__":
  main()
