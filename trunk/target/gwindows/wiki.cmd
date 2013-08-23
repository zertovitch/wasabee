@echo off

if "%1"=="" echo please provide a valid Wikipedia topic (case-sensitive!)
if "%1"=="" pause

if not "%1"=="" w http://en.wikipedia.org/wiki/%1 
