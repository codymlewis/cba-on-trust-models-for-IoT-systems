#!/bin/sh

mal_start=0.55
mal_end=$mal_start

# ./ConsoleInterface.r --bad_mouth --capability_set --total_nodes=200 --transactions=500 --malicious_start=$mal_start --malicious_end=$mal_end --targeted --type_calc=gnsq --disregard_multiplier=0.00001 & \
./ConsoleInterface.r --context_attack --total_nodes=200 --transactions=500 --malicious_start=$mal_start --malicious_end=$mal_end --targeted --type_calc=normal
# ./ConsoleInterface.r --context_attack --total_nodes=200 --transactions=500 --malicious_start=0.55 --malicious_end=0.55 --type_calc=normal & \
# ./ConsoleInterface.r --context_attack --total_nodes=200 --transactions=500 --malicious_start=0.30 --malicious_end=0.30 --type_calc=normal & \
# ./ConsoleInterface.r --context_attack --total_nodes=200 --transactions=500 --malicious_start=0.10 --malicious_end=0.10 --type_calc=normal & \
# ./ConsoleInterface.r --context_attack --total_nodes=200 --transactions=500 --malicious_start=0.20 --malicious_end=0.20 --type_calc=normal
