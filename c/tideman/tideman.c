// #include <cs50.h>
#include <stdio.h>
#include <string.h>
#include <ctype.h>
#include <stdbool.h>

// Max number of candidates
#define MAX 9

// preferences[i][j] is number of voters who prefer i over j
int preferences[MAX][MAX];

// locked[i][j] means i is locked in over j
bool locked[MAX][MAX];

// Each pair has a winner, loser
typedef struct
{
    int winner;
    int loser;
} pair;

// Array of candidates
char *candidates[MAX];
pair pairs[MAX * (MAX - 1) / 2];

int pair_count;
int candidate_count;

// Function prototypes
bool vote(int rank, char *name, int ranks[]);
void record_preferences(int ranks[]);
void add_pairs(void);
void sort_pairs(void);
void lock_pairs(void);
void print_winner(void);

int main(int argc, char *argv[])
{
    // Check for invalid usage
    if (argc < 2)
    {
        printf("Usage: tideman [candidate ...]\n");
        return 1;
    }

    // Populate array of candidates
    candidate_count = argc - 1;
    if (candidate_count > MAX)
    {
        printf("Maximum number of candidates is %i\n", MAX);
        return 2;
    }
    for (int i = 0; i < candidate_count; i++)
    {
        candidates[i] = argv[i + 1];
    }

    // Clear graph of locked in pairs
    for (int i = 0; i < candidate_count; i++)
    {
        for (int j = 0; j < candidate_count; j++)
        {
            locked[i][j] = false;
        }
    }

    pair_count = 0;
    int voter_count = get_int("Number of voters: ");

    // Query for votes
    for (int i = 0; i < voter_count; i++)
    {
        // ranks[i] is voter's ith preference
        int ranks[candidate_count];

        // Query for each rank
        for (int j = 0; j < candidate_count; j++)
        {
            char *name = get_string("Rank %i: ", j + 1); // Rank 1(0): Charlie; Rank 2 (1): Alice; Rank 3 (2): Bob;

            if (!vote(j, name, ranks)) // (0, "Charlie", ranks) (1, "Alice", ranks)
            {
                printf("Invalid vote.\n");
                return 3;
            }
        }

        record_preferences(ranks);

        printf("\n");
    }

    add_pairs();
    sort_pairs();
    lock_pairs();
    print_winner();
    return 0;
}

// Update ranks given a new vote
bool vote(int rank, char *name, int ranks[])
{
    name[0] = toupper(name[0]); // (0, "Charlie", ranks)(1, "Alice", ranks)

    for (int i = 0; i < candidate_count; i++) // loop through all of the candidates inputed at argv [Charlie: i = 2], [Alice: i=0]
    {
        if (strcmp(name, candidates[i]) == 0) // if the vote is equal to one of the candidates array values
        {
            ranks[rank] = i; // assign the postion of the current name to the rank position in the array ranks -- ranks[0] = 2, ranks[1] = 0 ... ranks = [2,0,1]
            return true;
        }
    }
    return false;
}

// Update preferences given one voter's ranks
void record_preferences(int ranks[]) // [2,0,1]
{
    for (int i = 0; i < candidate_count; i++) // ranks[0] = 2 --- i = 1; ranks[1] = 0
    {
        for (int j = i + 1; j < candidate_count; j++) // ranks[1] = 0, ranks[2] = 1 --- i = 1, j = 2 = ranks[2] = 1;
        {
            preferences[ranks[i]][ranks[j]]++; // preferences[2][0]++; preferences[2][1]++; ... preferences[0][1]++
        }
    }
}

// Record pairs of candidates where one is preferred over the other
void add_pairs(void)
{
    int i;
    int j;

    for (i = 0; i < candidate_count - 1; i++)
    {
    }
}

// Sort pairs in decreasing order by strength of victory
void sort_pairs(void)
{
    // TODO
    return;
}

// Lock pairs into the candidate graph in order, without creating cycles
void lock_pairs(void)
{
    // TODO
    return;
}

// Print the winner of the election
void print_winner(void)
{
    // TODO
    return;
}
