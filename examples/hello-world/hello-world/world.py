#!/usr/bin/env python

import copy
import json

def main():
  with open('/app/data/control.json', 'r') as control_file:
    control_data = json.load(control_file)
    print '\n', control_data, '\n'

  with open('/app/data/input.json', 'r') as input_file:
    input_data = json.load(input_file)
    print '\n', input_data, '\n'

  print '\n', "world worker", '\n'
  output_data = copy.deepcopy(input_data)
  output_data['step-2'] = 'world'
  store_data = copy.deepcopy(output_data)
  store_data['storage'] = 'true'

  print '\n', output_data, '\n'
  with open('/app/data/output.json', 'w') as output_file:
    json.dump(output_data, output_file)

  print '\n', store_data, '\n'
  with open('/app/store/world.json', 'w') as store_file:
    json.dump(store_data, store_file)

if __name__ == "__main__":
  main()
