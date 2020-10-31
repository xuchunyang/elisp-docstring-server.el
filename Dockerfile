FROM silex/emacs
ADD * /root/
WORKDIR /root
EXPOSE 3000
CMD ./starter.el
