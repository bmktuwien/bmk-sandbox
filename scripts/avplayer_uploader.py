#!/usr/bin/env python

from __future__ import unicode_literals

import os
import re
import requests
import sys
import urlparse

# globals
video_types = ['mp4', 'avi', 'xvid', 'mkv']


# print the usage message and die
def print_usage():
    print('avplayer_uploader <url> <directory>')
    sys.exit(1)


def bail_out(msg):
    print('Error: %s' % msg)
    sys.exit(1)


# return video file names in the given directory
def get_video_files(root_dir):
    global video_types

    for directory, _, files in os.walk(root_dir):
        for f in files:
            vf_types = [t for t in video_types if f.endswith('.'+t)]
            if vf_types:
                yield os.path.join(directory, f), f, vf_types[0]


# Upload video with the multipart/data request.
# Don't forget to append 'Submit' in the end so that the
# server accepts the request
def upload_video(url, file_path, file_name, file_type):
    files = [
        ('file', (file_name, open(file_path, 'rb'), 'video/'+file_type)),
        ('file', ('button', 'Submit'))
    ]

    print('Uploading %s...' % file_name)
    response = requests.post(url, files=files)

    if response.status_code == 200:
        print('%s successfully uploaded! :-)' % file_name)
    else:
        print('Something went wrong: %s :-(' % response.reason)


def main(args=sys.argv[1:]):
    if len(args) != 2:
        print_usage()

    url, working_dir = args

    # check if working_dir exists and if it is a dir
    if not os.path.exists(working_dir):
        bail_out('%s does not exist' % working_dir)

    if not os.path.isdir(working_dir):
        bail_out('%s is not a directory' % working_dir)

    # now check url
    if not re.search(r'^.+://', url):
        url = 'http://%s' % url

    url_parsed = urlparse.urlparse(url)
    if url_parsed.scheme != 'http':
        bail_out('Only http scheme is supported')
    if not url_parsed.netloc:
        bail_out('Invalid url %s' % url)

    for fp, fn, ft in get_video_files(working_dir):
        upload_video(url, fp, fn, ft)


if __name__ == '__main__':
    main()
