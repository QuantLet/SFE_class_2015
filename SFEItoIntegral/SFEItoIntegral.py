import numpy as np
import matplotlib.pyplot as plt

def SFEItoIntegral(dt=0.0001, c=1, set_seed=True):
    if dt <= 0:
        raise ValueError("Delta t must be strictly larger than 0!")
    
    if set_seed:
        np.random.seed(5)

    # Calculation of basic parameters
    n = int(1 / dt)
    t = np.linspace(0, 1, n + 1)  # Includes 0 and 1

    # Generate Wiener process
    w = np.random.rand(n)            # Uniform samples between 0 and 1
    w = 2 * (w > 0.5) - 1            # Convert to -1 or 1
    dx = c * np.sqrt(dt)
    w = w * dx
    w = np.cumsum(w)
    
    # Itô integral calculation
    w_sq = w**2
    ito_integral = (w_sq - t[1:]) / 2

    # Combine for plotting
    output = np.column_stack((ito_integral, w))

    # Plotting
    plt.figure(figsize=(8, 6))
    plt.plot(t[1:], output[:, 0], label=r"$\int_0^t W_s dW_s$", color='red', linewidth=2)
    plt.plot(t[1:], output[:, 1], label=r"$W_t$", color='blue', linewidth=2)

    plt.title("Ito Integration")
    plt.xlabel("Time t")
    plt.ylabel("Values of the Wiener process and the Ito integral")
    plt.xticks(np.linspace(0, 1, 5), [f"{x:.2f}" for x in np.linspace(0, 1, 5)])
    plt.legend(loc="upper right", fontsize=12)
    plt.grid(True)
    plt.tight_layout()

    # Save plot
    plt.savefig("SFEItoIntegral.png")
    plt.show()
    plt.close()

# Call the function
SFEItoIntegral(dt=0.0001, c=1)
