"Socket activation" is the a feature of systemd. ([documentation])

We use it for web servers, to avoid the momentary downtime that otherwise occurs
while restarting processes. Because the socket is manged by systemd, not by our
process, the socket remains even while our process is down. Requests to the
socket are queued until our process comes back up to respond.

  [documentation]: https://www.freedesktop.org/software/systemd/man/sd_listen_fds_with_names.html
