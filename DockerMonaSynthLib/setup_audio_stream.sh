#!/bin/bash

# Install Icecast2 and DarkIce if they are not installed already
apt-get update && apt-get install -y icecast2 darkice

# Create the Icecast configuration
cat <<EOL > /etc/icecast2/icecast.xml
<icecast>
    <limits>
        <clients>100</clients>
        <sources>2</sources>
        <queue-size>524288</queue-size>
        <client-timeout>30</client-timeout>
        <header-timeout>15</header-timeout>
        <source-timeout>10</source-timeout>
        <burst-on-connect>1</burst-on-connect>
        <burst-size>65535</burst-size>
    </limits>

    <authentication>
        <admin>
            <username>admin</username>
            <password>hackme</password>
        </admin>
    </authentication>

    <hostname>localhost</hostname>
    <listen-socket>
        <port>8000</port>
    </listen-socket>

    <paths>
        <logdir>/var/log/icecast</logdir>
        <webroot>/usr/share/icecast/web</webroot>
        <adminroot>/usr/share/icecast/admin</adminroot>
        <pidfile>/run/icecast/icecast.pid</pidfile>
    </paths>

    <mount>
        <mount-name>/stream</mount-name>
        <file>/usr/share/icecast/audio.mp3</file>
    </mount>

    <logging>
        <accesslog>/var/log/icecast/access.log</accesslog>
        <errorlog>/var/log/icecast/error.log</errorlog>
        <loglevel>3</loglevel>
        <logsize>10000</logsize>
    </logging>
</icecast>
EOL

# Create the DarkIce configuration
cat <<EOL > /etc/darkice.cfg
[general]
duration        = 0        # duration in seconds, 0 means forever
bufferSecs      = 5        # size of internal slip buffer, in seconds

[input]
device          = default  # ALSA soundcard device for the input stream
sampleRate      = 44100    # sample rate in Hz. Default is 44100
bitsPerSample   = 16       # convert to 16-bit integer (browser compatibility)
channel         = 2        # number of channels. 1 = mono, 2 = stereo

[icecast2-0]
bitrateMode     = cbr       # constant bit rate
bitrate         = 128       # bitrate of the stream in kbps
format          = mp3       # format of the stream: ogg vorbis or mp3
server          = localhost # Icecast server to connect to
port            = 8000      # port of the Icecast server
password        = hackme    # source password for the Icecast server
mountPoint      = stream    # mount point where to send the stream
name            = Docker Audio Stream
EOL

# Start Icecast and DarkIce
icecast2 -c /etc/icecast2/icecast.xml & darkice -c /etc/darkice.cfg

