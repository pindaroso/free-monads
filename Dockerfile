FROM pindaroso/stack:latest
WORKDIR /home/robot/code
ADD . .
RUN stack setup
RUN stack build
