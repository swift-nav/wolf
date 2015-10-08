#!/usr/bin/env python

import json

def main():
  with open('/app/data/control.json', 'r') as control_file:
    control_data = json.load(control_file)
    print '\n', control_data, '\n'
    with open('/app/data/input.json', 'r') as input_file:
      input_data = json.load(input_file)
      print '\n', input_data, '\n'

if __name__ == "__main__":
  main()
