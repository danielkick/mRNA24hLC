import pandas as pd
from brian2 import *
#%matplotlib inline

# Read in temp files from R -----
tau_file = open("./temp/measured_tau.txt")
tau_file = tau_file.read()

Rin_file = open("./temp/measured_Rin.txt")
Rin_file = Rin_file.read()

# Set run params ----
# M_file = pd.read_csv(read_this_file)
M_file = pd.read_csv("./temp/current_inj.csv")
#M_file = pd.read_csv("epsp_inj.csv")

measured_tau = float(tau_file)

measured_Rin = float(Rin_file)

my_current = M_file.Inj # Note that this must be in the temp file
my_current2 = my_current.to_numpy()

Max_ms = max(M_file.Time)*1000


start_scope()
tau = measured_tau*ms

#I_recorded = TimedArray(my_current2, dt=defaultclock.dt) # scale array and make into a Brian Class
#eqs = '''
#dv/dt = (I-v)/tau : 1
#I = I_recorded(t) : 1
#'''

#original does not contain this. This _should_ give use the same number of samples out as went in.
# defaultclock.dt = 2*ms
defaultclock.dt = (Max_ms/len(M_file))*ms


V_expected = my_current2*measured_Rin
V_expected2 = TimedArray(V_expected, dt=defaultclock.dt) # scale array and make into a Brian Class

eqs = '''
dv/dt = (Vexp-v)/tau : 1
Vexp = V_expected2(t) : 1
'''

G = NeuronGroup(1, eqs,
                #threshold='v>1', reset='v=0',
                method='exact')
M = StateMonitor(G, variables=True, record=True)

# Run simulation ----


run(Max_ms*ms)
#plot(M.t/ms, M.Vexp[0], label='V Expected')
#plot(M.t/ms, M.v[0], label='v')
#xlabel('Time (ms)')
#ylabel('v')
#legend(loc='best');

# Write out results to be used by R ----
predicted_response = M.v[0]
pd.DataFrame(predicted_response).to_csv("./temp/predicted_voltage.csv", header = ["predicted"], index = False)

pd.DataFrame(np.array(M.t)).to_csv("./temp/time_steps.csv", header = ["Time"], index = False)







