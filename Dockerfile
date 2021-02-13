FROM voidlinux/voidlinux:latest

RUN xbps-install -Suy && xbps-install -y erlang curl git gcc

RUN cd /usr/bin && curl -LJO https://github.com/erlang/rebar3/releases/download/3.14.3/rebar3 && chmod a+wrx rebar3

# delete all the apt list files since they're big and get stale quickly
RUN rm -rf /var/lib/apt/lists/*
# this forces "apt-get update" in dependent images, which is also good
# (see also https://bugs.launchpad.net/cloud-images/+bug/1699913)

# make systemd-detect-virt return "docker"
# See: https://github.com/systemd/systemd/blob/aa0c34279ee40bce2f9681b496922dedbadfca19/src/basic/virt.c#L434
RUN mkdir -p /run/systemd && echo 'docker' > /run/systemd/container

ADD ./ /rbot/

EXPOSE 9418
EXPOSE 80
EXPOSE 443
EXPOSE 8080
EXPOSE 8443

RUN cd /rbot/ && rebar3 compile

CMD cd /rbot/ && rebar3 shell
