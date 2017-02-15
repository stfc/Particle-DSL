#!/usr/bin/env python3

def S(r,rc,l):
    if r<rc-l:
        return 1.0
    elif r<=rc:
        ar=(r-rc+l)/l
        return 1+ar*ar*(2*ar-3)
    else:
        return 0.0
def dS(r,rc,l):
    if r<rc-l:
        return 0.0
    elif r<=rc:
        ar=1.0/l
        return 6*ar*(ar-1)
    else:
        return 0.0


def lj(r,sig,eps):
    return 4.0*eps*((sig/r)**12-(sig/r)**6)

def dlj(r,sig,eps):
    return 4.0*eps*(-12.0*(sig/r)**13/sig+6.0*(sig/r)**6/sig)

def ljS(r,sig,eps,rc,l):
    return lj(r,sig,eps)*S(r,rc,l)

def dljS(r,sig,eps,rc,l):
    return dlj(r,sig,eps)*S(r,rc,l)+lj(r,sig,eps)*dS(r,rc,l)

R=15.0
sig=3.405
eps=119.8
rc=3*sig
l=1.0
n=1000
a=0.97*sig
h=(R-a)/n
g=open("lj.dat","w")
f=open("dlj.dat","w")
for i in range(0,n+1):
    r=a+i*h
    f.write("%16.8e %16.8e %16.8e\n"%(r,dlj(r,sig,eps),dljS(r,sig,eps,rc,l)))
    g.write("%16.8e %16.8e %16.8e\n"%(r,lj(r,sig,eps),ljS(r,sig,eps,rc,l)))
f.close()    
g.close()    
