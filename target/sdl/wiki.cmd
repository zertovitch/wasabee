@echo off

if "%1"=="" echo please provide a valid Wikipedia topic (case-sensitive!)
if "%1"=="" pause

if not "%1"=="" w http://%1.wikipedia.org/wiki/%2 
