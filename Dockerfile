FROM voidlinux/voidlinux:latest

RUN mkdir -p /run/systemd && echo 'docker' > /run/systemd/container

RUN xbps-install -Suy erlang curl git gcc
RUN cd /usr/bin && curl -LJO https://github.com/erlang/rebar3/releases/download/3.14.3/rebar3 && chmod a+x rebar3

ADD ./ /rbot/

EXPOSE 80
EXPOSE 443
EXPOSE 8080
EXPOSE 8443
EXPOSE 9418

RUN cd /rbot/ && rebar3 release

CMD /rbot/_build/default/rel/prod/bin/prod foreground

