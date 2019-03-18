FROM raw996/dhadley:3.5.0

RUN mkdir ~/.ssh/
RUN ssh-keyscan github.com >> ~/.ssh/known_hosts

## LOADING IN DASHBOARDS FROM GIT

RUN mkdir -p $HOME/git/

ADD repo-key /
RUN \
  chmod 600 /repo-key && \  
  echo "IdentityFile /repo-key" >> /etc/ssh/ssh_config && \  
  echo "StrictHostKeyChecking no" >> /etc/ssh/ssh_config && \  
  cd $HOME/git/ && \
  git clone https://github.com/raubreywhite/internal_surveillance.git
RUN rm /repo-key
RUN mkdir -p /git/internal_surveillance/dofiles/
RUN cp -r $HOME/git/internal_surveillance/dofiles/* /git/internal_surveillance/dofiles/

## COMPUTER NAME
