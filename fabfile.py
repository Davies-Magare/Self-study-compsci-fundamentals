from fabric.api import *

def commit(message):
    local("git add .")
    local(f"git commit -m '{message}'")
    local("git push")
