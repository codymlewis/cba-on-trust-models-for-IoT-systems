#!/usr/bin/env python3

import argparse
import matplotlib.pyplot as plt

def find_ci(theta, time1, timei):
    return theta**(time1 - timei)

def find_n_qr(qrs, times, theta, cxf):
    numerator = 0
    denominator = 0
    for qr, time in zip(qrs, times):
        ci = find_ci(theta, times[0], time)
        numerator += ci * qr + cxf
        denominator += ci + abs(cxf)
    return numerator / denominator

if __name__ == '__main__':
    PARSER = argparse.ArgumentParser(description="Show how the QR calculation changes over time")
    PARSER.add_argument("-e", "--epochs", dest="epochs", type=int,
                        action="store", default=500,
                        help="The number of epochs to run for [default 500]")
    PARSER.add_argument("-u", "--update", dest="update", type=float,
                        action="store", default=0,
                        help="Amount to update the QR by at each epoch")
    ARGS = PARSER.parse_args()
    QRS = [1]
    TIMES = [0]
    THETA = 0.7
    for i in range(0, ARGS.epochs):
        QRS.insert(0, find_n_qr(QRS, TIMES, THETA, ARGS.update))
        TIMES.insert(0, i)

    QRS.reverse()
    TIMES.reverse()
    plt.plot(TIMES, QRS)
    FILENAME = "qrchanges.png"
    plt.savefig(FILENAME)
    print(f"Plot saved to {FILENAME}")
    plt.show()
