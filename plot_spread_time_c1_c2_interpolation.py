# plotting theory simulations for c_1 c_2 interpolation (Theorem 3).


from models import *

from computing_spread_time_c1_c2_interpolation import etas, q_labels, q_labels_old, qs, qs_old, size_of_dataset, \
    network_size, q_labels_new, qs_new

all_q_labels = q_labels_old + q_labels + q_labels_new

all_qs = qs_old[::-1] + qs[::-1] + qs_new[::-1]

if __name__ == '__main__':

    assert do_plots and load_computations, "we should be in load_computations and do_plots mode!"

    assert (simulation_type == 'c1_c2_interpolation') \
           or (simulation_type =='c1_c2_interpolation_SimpleOnlyAlongC1'), \
        "we are not in the right simulation_type:" + simulation_type

    avg_spread_times = pickle.load(open(theory_simulation_pickle_address
                                        + 'spreading_time_avg_'
                                        + simulation_type
                                        + '.pkl', 'rb'))
    std_spread_times = pickle.load(open(theory_simulation_pickle_address
                                        + 'spreading_time_std_'
                                        + simulation_type
                                        + '.pkl', 'rb'))

    plt.figure()

    for q_label in all_q_labels:

        print(etas)
        print(avg_spread_times[all_q_labels.index(q_label)])

        plt.errorbar(etas, avg_spread_times[all_q_labels.index(q_label)],
                     yerr=[1.96 * s / np.sqrt(size_of_dataset) for s in std_spread_times[all_q_labels.index(q_label)]],
                     linewidth=2.5, label='$q = '+str(Decimal(all_qs[all_q_labels.index(q_label)]).quantize(FOURPLACES))
                                          +'$')

    plt.ylabel('time to spread')
    plt.xlabel('edges rewired ($\\eta$)')
    plt.title('\centering Complex Contagion over $\mathcal{C}_2^{\eta}, n = ' + str(network_size) + '$'
              '\\vspace{-10pt}  \\begin{center}  with Sub-threshold Adoptions $(q)$ '
              'and Rewiring $(\eta)$   \\end{center}')
    plt.legend(loc='lower right', bbox_to_anchor=(1, 0.065),fontsize=11)
    if show_plots:
        plt.show()
    if save_plots:
        plt.savefig(theory_simulation_output_address + simulation_type + '.png')