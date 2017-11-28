FROM spacemacs/emacs25:develop

RUN groupadd ${GNAME} \
    && useradd -ms /bin/bash -d ${UHOME} -g ${GNAME} ${UNAME} \
    && apt-get update \
    && apt-get install sshpass \
                       fonts-inconsolata \
    && apt autoremove

COPY --chown=spacemacser:emacs conf ${UHOME}
COPY --chown=spacemacser:emacs emacs/conf ${UHOME}
COPY --chown=spacemacser:emacs emacs/rootfs /

RUN rm ${UHOME}/.spacemacs \
    && asEnvUser emacs --batch --load=${UHOME}/.emacs.d/init.el \
    && asEnvUser ssh-keygen -t rsa -f ${UHOME}/.ssh/id_rsa -N ''

VOLUME /home/emacs/code
WORKDIR ${UHOME}
CMD ["bash", "-c", "cat"]
