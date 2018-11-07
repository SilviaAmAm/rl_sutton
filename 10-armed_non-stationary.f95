program TestBed
    implicit none

    ! This program is an implementation of the section 2.5 armed bandit test bed where the real action values are not
    ! stationary. Taken from the book:
    ! "Reinforcement Learning: An Introduction" by Richard S. Sutton and Andrew G. Barto

    ! Generate 2000 k-armed bandit problems with k = 10

    ! The real action values are chosen from a Gaussian distribution with mean 0 and variance 1
    ! q*(a) = N(0, 1) with a = 1, .., 10

    ! When a learning method choses the action At at time t, the reward is chosen from a Gaussian with mean q*(At) and variance 1
    ! Rt = N(q*(At), 1)

    ! After each step, the q* values are modified by adding a normally distributed increment with mean 0 and std 0.01

    ! Each learning method learns for 1000 time steps, and this is repeated for 2000 times

    !-------------------- * Declaring the variables * ------------------------

    real, allocatable, dimension(:) :: q_star, reward_given, Q, N
    integer :: i, j,  n_steps, n_test, k, A, counter
    real :: epsilon, reward, alpha


    !-------------------- * Initialising the variables * ------------------------

    ! The number of actions that can be chosen
    k = 10

    ! The step size parameter for the exponentially weighted averages
    alpha = 0.1

    ! The number of times to chose an action:
    n_steps = 10000
    n_test = 2000

    ! The epsilon value for the epsilon greedy algorithm
    epsilon = 0.9

    ! The 'real' action value for an action:
    allocate(q_star(k))

    ! Initialising the array to store the results
    allocate(reward_given(n_steps))

    do j=1,n_steps
        reward_given(j) = 0
    end do

    ! The estimated action value for an action:
    allocate(Q(k))

    ! The number of times that action has been chosen:
    allocate(N(k))

    !-------------------- * Main bandit loop * ------------------------
    do counter = 1, n_test

        ! Re-initialising the estimate for the action value and the number of times each action has been picked
        do i = 1, k
            Q(i) = 0
            N(i) = 0
        end do

        ! Re-initialising the values of q_star
        call initialise_action_value(q_star, k)

        ! Main Learning loop
        do i = 1, n_steps

            call chose_action_eps_greedy(Q, k, epsilon, A)
            call calculate_reward(A, k, q_star, reward)
            call modify_q_star(q_star, k)

            reward_given(i) = reward_given(i) + reward

            !Update N and Q
            N(A) = N(A) + 1
            ! Sample average
            Q(A) = Q(A) + 1/(N(A)) * (reward - Q(A))
            ! Exponentially weighted average
            ! Q(A) = Q(A) + alpha * (reward - Q(A))

        end do
    end do

    open(21,file="results_sample_avg.csv")
    !open(21,file="results_exp_avg.csv")

    do j = 1, n_steps
        reward_given(j) = reward_given(j) / n_test
        write(21, *) j,",", reward_given(j)
    end do

    close(21)


end program

!-------------------- * Subroutines * ------------------------

! This subroutine initialises the action values for each of the actions from a Gaussian distribution of mean 0 and
! variance 1
subroutine initialise_action_value(action_val, n_actions)
    implicit none

    integer, intent(in) :: n_actions
    real, dimension(n_actions), intent(out) :: action_val
    integer :: i
    real :: r1, r2
    real, parameter ::  pi=3.14159265359

    do i = 1, n_actions
        ! Uniform random numbers
        call random_number(r1)
        call random_number(r2)

        !Turning them into gaussian sampled (mean 0 and std 1)
        action_val(i) = SQRT(-2.0 * LOG(r1)) * COS(2 * pi * r2)

    end do
end subroutine

! This subroutine uses the epsilon greedy algorithm to chose the next action
subroutine chose_action_eps_greedy(Q, k, epsilon, A)
    implicit none

    integer, intent(in) :: k
    real, intent(in) :: epsilon
    real, dimension(k), intent(in) :: Q
    integer, intent(out) :: A
    real :: rand_n1, rand_n2, max_Q
    integer :: ii

    ! Generating a random number
    call random_number(rand_n1)

    ! If the number is smaller than epsilon chose the action with larger Q (Exploit)
    if (rand_n1 <= epsilon) then

        A = 0
        max_Q = 0

        ! Find which action has the largest Q
        do ii = 1, k
            if (max_Q < Q(ii)) then
                max_Q = Q(ii)
                A = ii
            end if
        end do

    ! Otherwise, choose a random action (Explore)
    else if (rand_n1 > epsilon) then
        call random_number(rand_n2)

        A = 1 + FLOOR((k)*rand_n2)
    end if

end subroutine


! This subroutine calculates the reward for each action chosen based on the real action value
subroutine calculate_reward(A, k, action_value, reward)
    implicit none

    integer, intent(in) :: A, k
    real, dimension(k), intent(in) :: action_value
    real, intent(out) :: reward
    real :: r1, r2, g1
    real, parameter ::  pi=3.14159265359

    ! Uniform random numbers
    call random_number(r1)
    call random_number(r2)

    !Turning it into gaussian sampled (mean 0 and std 1)
    g1 = SQRT(-2.0 * LOG(r1)) * COS(2 * pi * r2)

    ! Turning it into gaussian sampled (mean action_value, std 1)
    reward = g1 + action_value(A)

end subroutine

subroutine modify_q_star(q_star, k)
    implicit none

    integer, intent(in) :: k
    real, dimension(k), intent(inout) :: q_star
    real :: r1, r2, g1
    integer :: counter
    real, parameter ::  pi=3.14159265359

    do counter = 1, k
        ! Uniform random numbers
        call random_number(r1)
        call random_number(r2)

        ! Turning it into gaussian sampled (mean 0 and std 1)
        g1 = SQRT(-2.0 * LOG(r1)) * COS(2 * pi * r2)

        ! Turning it into gaussian sampled (mean 0 and std 0.01)
        g1 = 0.1 * g1

        ! Modifying q*
        q_star(counter) = q_star(counter) + g1
    end do

end subroutine