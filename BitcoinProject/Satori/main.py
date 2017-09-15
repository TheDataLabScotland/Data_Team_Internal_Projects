#!/usr/bin/env python
# Dependencies
from __future__ import print_function
from satori.rtm.client import make_client, SubscriptionMode
import sys
import time

# Load credentials
appkey = '717fD4181fcCE4fb9c9ae758aC1aF3e2'
endpoint = 'wss://open-data.api.satori.com'

# Channel
channel = 'cryptocurrency-market-data'

def main():
    with make_client(endpoint=endpoint, appkey=appkey) as client:
        print('Connected to Satori RTM!')

        class SubscriptionObserver(object):
            def on_subscription_data(self, data):
                for message in data['messages']:
                    print("Got message:", message)

        subscription_observer = SubscriptionObserver()
        client.subscribe(
            channel,
            SubscriptionMode.SIMPLE,
            subscription_observer)

        try:
            while True:
                time.sleep(1)
        except KeyboardInterrupt:
            pass


if __name__ == '__main__':
    main()
