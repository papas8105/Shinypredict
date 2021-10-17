
# Fabric library to send commands to Raspberry pi cluster
# 1. First wrap up all all Raspberry pi cluster's IP using env.host()
# 2. Then send any commands to the cluster using fab cmd "command"

# CLUSTER 3 RASPBERRY MODEL 8 8GB RAM


from fabric.api import *
    env.host = [

        'pi@192.168.1.64',
        'pi@192.168.1.64',
        'pi@192.168.1.64',

    ]
    env.password='raspberry'


    #dasds
    ##dasd
    ## fab cmd:"echo pi:pai1978|chpasswd"
    ## echo pi:t0p3cr3t|chpasswd
    ## fab cmd "apt update"
    ## fab cmd "apt upgrade"
    ## fab cmd "apt dist-upgrade-y"
    ## fab cmd "raspi-config--expand--rootfs"
    ## fab cmd "reboot now"

    