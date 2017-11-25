FROM rastasheep/ubuntu-sshd:16.04

ENV UNAME=spacemacser
ENV GNAME=emacs
ENV UHOME=/home/emacs

RUN apt-get update \
    && apt-get install -y sudo \
                          git \
    && apt autoremove \
    && groupadd ${GNAME} \
    && useradd -ms /bin/bash -d ${UHOME} -g ${GNAME} ${UNAME} \
    && echo spacemacser:password | chpasswd \
    && echo "${UNAME} ALL=(ALL) NOPASSWD:ALL" >> /etc/sudoers

COPY --chown=spacemacser:emacs conf ${UHOME}
USER ${UNAME}
VOLUME /home/emacs/code
ENTRYPOINT ["bash", "-c", "ssh-keygen -A; sudo /usr/sbin/sshd -D -e"]
