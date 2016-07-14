""" Stochastic gradient descent."""
import math
import numpy as np

N_FEATURES = 2

def gradient(regularization_parameter,
             learning_rate, difference, model,
             features):
    """ The gradient """
    model[0] = model[0] - learning_rate * difference
    model[1:] = model[1:] - learning_rate * (difference * features + regularization_parameter * model[1:])
    return(model)

def stochastic_gradient_descent(
        hypothesis,
        regularization_parameter,
        learning_rate,
        example,
        model):
    """ A stochastic_gradient_descent step. """
    (features, target) = example
    approximation = hypothesis(model, features)
    difference = approximation - target
    cost = -(math.log(approximation)*target + math.log(1-approximation)*(1-target))
    next_model = gradient(regularization_parameter,
                          learning_rate, difference, model,
                          features)
    return (cost, next_model)

def sigmoid_hypothesis(model, features):
    """ The sigmoid function """
    return(1 / (1 + math.exp (-(np.dot(model[1:], features) + model[0]))))

def examples():
    """ Generator of examples """
    n_examples = 1
    while n_examples < 1000000:
        features = np.random.rand(N_FEATURES)
        if features[1] > 0.5:
            target = 1
        else:
            target = 0
        yield(features, target)
        n_examples += 1

if __name__ == "__main__":
    (err, model) = (0, np.zeros(N_FEATURES + 1))
    for example in examples():
        #print(example)
        (err, model) = stochastic_gradient_descent(
            sigmoid_hypothesis,
            0.01,
            0.1,
            example,
            model)
    print((err,model))
