#include <openssl/ec.h>
#include <openssl/sha.h>
#include <openssl/obj_mac.h>

#include <stdio.h>
#include <stdlib.h>
#include <stdint.h>
#include <string.h>

#include <time.h>

int curve_id;
size_t FHMQV_PUBKEY_UNCOMPRESSED_SIZE;
size_t FHMQV_PRIVKEY_SIZE;


void hash(const void *e0, size_t e0_len,
          const void *e1, size_t e1_len,
          const void *e2, size_t e2_len,
          const void *e3, size_t e3_len,
          const void *e4, size_t e4_len,
          uint8_t *result, size_t result_len)
{
    size_t len;
    SHA512_CTX sha_ctx;
    unsigned char md[SHA384_DIGEST_LENGTH];

    SHA384_Init(&sha_ctx);

    if (e0 != NULL) {
        SHA384_Update(&sha_ctx, e0, e0_len);
    }

    SHA384_Update(&sha_ctx, e1, e1_len);
    SHA384_Update(&sha_ctx, e2, e2_len);
    SHA384_Update(&sha_ctx, e3, e3_len);
    SHA384_Update(&sha_ctx, e4, e4_len);

    SHA384_Final(md, &sha_ctx);

    len = result_len < SHA384_DIGEST_LENGTH ? result_len : SHA384_DIGEST_LENGTH;

    memcpy(result, md, len);
}

int fhmqv_calculate_shared_key(const uint8_t *e_privkey, size_t e_privkey_len,
                               const uint8_t *e_pubkey, size_t e_pubkey_len,
                               const uint8_t *s_privkey, size_t s_privkey_len,
                               const uint8_t *s_pubkey, size_t s_pubkey_len,
                               const uint8_t *other_e_pubkey, size_t other_e_pubkey_len,
                               const uint8_t *other_s_pubkey, size_t other_s_pubkey_len,
                               uint8_t *shared, size_t shared_len, int server)
{

    EC_GROUP *group;
    BIGNUM *e_privkey_bn = BN_new(), *s_privkey_bn = BN_new();
    BIGNUM *q = BN_new(), *d = BN_new(), *e = BN_new();
    BIGNUM *s = BN_new(), *tmp = BN_new();
    BN_CTX *bn_ctx = BN_CTX_new();

    EC_POINT *e_pubkey_point, *other_e_pubkey_point, *other_s_pubkey_point;
    EC_POINT *sigma, *t1, *t2;

    size_t len;
    size_t sigma_len;
    uint8_t *d_bin, *e_bin;
    uint8_t *sigma_bin;

    group = EC_GROUP_new_by_curve_name(curve_id);
    EC_GROUP_get_order(group, q, NULL);
    len = BN_num_bytes(q) / 2;

    e_pubkey_point = EC_POINT_new(group);
    other_e_pubkey_point = EC_POINT_new(group);
    other_s_pubkey_point = EC_POINT_new(group);
    t1 = EC_POINT_new(group);
    t2 = EC_POINT_new(group);
    sigma = EC_POINT_new(group);


    BN_bin2bn(e_privkey, e_privkey_len, e_privkey_bn);
    BN_bin2bn(s_privkey, s_privkey_len, s_privkey_bn);
    EC_POINT_oct2point(group, e_pubkey_point, e_pubkey, e_pubkey_len, NULL);
    EC_POINT_oct2point(group, other_e_pubkey_point, other_e_pubkey, other_e_pubkey_len, NULL);
    EC_POINT_oct2point(group, other_s_pubkey_point, other_s_pubkey, other_s_pubkey_len, NULL);

    if (!EC_POINT_is_on_curve(group, other_e_pubkey_point, NULL)) {
        return 1;
    }

    d_bin = malloc(sizeof(uint8_t) * len);
    e_bin = malloc(sizeof(uint8_t) * len);
    sigma_bin = malloc(sizeof(uint8_t) * FHMQV_PUBKEY_UNCOMPRESSED_SIZE);

    if (server) {
        hash(NULL, 0,
             other_e_pubkey, other_e_pubkey_len,
             e_pubkey, e_pubkey_len,
             other_s_pubkey, other_s_pubkey_len,
             s_pubkey, s_pubkey_len,
             d_bin, len);

        hash(NULL, 0,
             e_pubkey, e_pubkey_len,
             other_e_pubkey, other_e_pubkey_len,
             other_s_pubkey, other_s_pubkey_len,
             s_pubkey, s_pubkey_len,
             e_bin, len);


        BN_bin2bn(d_bin, len, d);
        BN_bin2bn(e_bin, len, e);

        BN_mul(tmp, e, s_privkey_bn, bn_ctx);
        BN_mod_add(s, e_privkey_bn, tmp, q, bn_ctx);

        EC_POINT_mul(group, t1, NULL, other_s_pubkey_point, d, NULL);
        EC_POINT_add(group, t2, other_e_pubkey_point, t1, NULL);
        EC_POINT_mul(group, sigma, NULL, t2, s, NULL);


        EC_POINT_point2oct(group, sigma, POINT_CONVERSION_UNCOMPRESSED,
                           sigma_bin, FHMQV_PUBKEY_UNCOMPRESSED_SIZE, NULL);

        hash(sigma_bin, FHMQV_PUBKEY_UNCOMPRESSED_SIZE,
             other_s_pubkey, other_s_pubkey_len,
             s_pubkey, s_pubkey_len,
             other_e_pubkey, other_e_pubkey_len,
             e_pubkey, e_pubkey_len,
             shared, shared_len);
    } else {
        hash(NULL, 0,
             e_pubkey, e_pubkey_len,
             other_e_pubkey, other_e_pubkey_len,
             s_pubkey, s_pubkey_len,
             other_s_pubkey, other_s_pubkey_len,
             d_bin, len);

        hash(NULL, 0,
             other_e_pubkey, other_e_pubkey_len,
             e_pubkey, e_pubkey_len,
             s_pubkey, s_pubkey_len,
             other_s_pubkey, other_s_pubkey_len,
             e_bin, len);


        BN_bin2bn(d_bin, len, d);
        BN_bin2bn(e_bin, len, e);

        BN_mul(tmp, d, s_privkey_bn, bn_ctx);
        BN_mod_add(s, e_privkey_bn, tmp, q, bn_ctx);

        EC_POINT_mul(group, t1, NULL, other_s_pubkey_point, e, NULL);
        EC_POINT_add(group, t2, other_e_pubkey_point, t1, NULL);
        EC_POINT_mul(group, sigma, NULL, t2, s, NULL);


        EC_POINT_point2oct(group, sigma, POINT_CONVERSION_UNCOMPRESSED,
                           sigma_bin, FHMQV_PUBKEY_UNCOMPRESSED_SIZE, NULL);

        hash(sigma_bin, FHMQV_PUBKEY_UNCOMPRESSED_SIZE,
             s_pubkey, s_pubkey_len,
             other_s_pubkey, other_s_pubkey_len,
             e_pubkey, e_pubkey_len,
             other_e_pubkey, other_e_pubkey_len,
             shared, shared_len);
    }


    EC_GROUP_clear_free(group);
    BN_free(e_privkey_bn);
    BN_free(s_privkey_bn);

    BN_free(q);
    BN_free(d);
    BN_free(e);
    BN_free(s);
    BN_free(tmp);

    BN_CTX_free(bn_ctx);

    EC_POINT_free(e_pubkey_point);
    EC_POINT_free(other_e_pubkey_point);
    EC_POINT_free(other_s_pubkey_point);
    EC_POINT_free(sigma);
    EC_POINT_free(t1);
    EC_POINT_free(t2);

    free(d_bin);
    free(e_bin);
    free(sigma_bin);

    return 0;
}



int benchmark(int curve, char *desc, char *comment)
{
    curve_id = curve;

    EC_KEY *static_keypair_server = EC_KEY_new_by_curve_name(curve_id);
    EC_KEY_generate_key(static_keypair_server);
    EC_KEY *ephermeal_keypair_server = EC_KEY_new_by_curve_name(curve_id);
    EC_KEY_generate_key(ephermeal_keypair_server);

    EC_KEY *static_keypair_client = EC_KEY_new_by_curve_name(curve_id);
    EC_KEY_generate_key(static_keypair_client);
    EC_KEY *ephermeal_keypair_client = EC_KEY_new_by_curve_name(curve_id);
    EC_KEY_generate_key(ephermeal_keypair_client);


    EC_POINT *clnt_e_pubkey_point = EC_KEY_get0_public_key(ephermeal_keypair_client);
    BIGNUM *clnt_e_privkey_bn = EC_KEY_get0_private_key(ephermeal_keypair_client);

    FHMQV_PUBKEY_UNCOMPRESSED_SIZE = EC_POINT_point2oct(EC_KEY_get0_group(ephermeal_keypair_client),
                                                        clnt_e_pubkey_point,
                                                        POINT_CONVERSION_UNCOMPRESSED, NULL, 0, NULL);
    size_t clnt_e_privkey_len = BN_num_bytes(clnt_e_privkey_bn);

    //printf("pubkey size: %u\n", FHMQV_PUBKEY_UNCOMPRESSED_SIZE);
    //printf("privkey size: %u\n", FHMQV_PRIVKEY_SIZE);


    uint8_t clnt_e_pubkey[FHMQV_PUBKEY_UNCOMPRESSED_SIZE];
    EC_POINT_point2oct(EC_KEY_get0_group(ephermeal_keypair_client), clnt_e_pubkey_point,
                       POINT_CONVERSION_UNCOMPRESSED, clnt_e_pubkey, FHMQV_PUBKEY_UNCOMPRESSED_SIZE, NULL);

    uint8_t *clnt_e_privkey = malloc(clnt_e_privkey_len);
    BN_bn2bin(clnt_e_privkey_bn, clnt_e_privkey);



    EC_POINT *clnt_s_pubkey_point = EC_KEY_get0_public_key(static_keypair_client);
    BIGNUM *clnt_s_privkey_bn = EC_KEY_get0_private_key(static_keypair_client);
    size_t clnt_s_privkey_len = BN_num_bytes(clnt_s_privkey_bn);

    uint8_t clnt_s_pubkey[FHMQV_PUBKEY_UNCOMPRESSED_SIZE];
    EC_POINT_point2oct(EC_KEY_get0_group(static_keypair_client), clnt_s_pubkey_point,
                       POINT_CONVERSION_UNCOMPRESSED, clnt_s_pubkey, FHMQV_PUBKEY_UNCOMPRESSED_SIZE, NULL);

    uint8_t *clnt_s_privkey = malloc(clnt_s_privkey_len);
    BN_bn2bin(clnt_s_privkey_bn, clnt_s_privkey);

    /*********************/

    EC_POINT *server_e_pubkey_point = EC_KEY_get0_public_key(ephermeal_keypair_server);
    BIGNUM *server_e_privkey_bn = EC_KEY_get0_private_key(ephermeal_keypair_server);
    size_t server_e_privkey_len = BN_num_bytes(server_e_privkey_bn);

    uint8_t server_e_pubkey[FHMQV_PUBKEY_UNCOMPRESSED_SIZE];
    EC_POINT_point2oct(EC_KEY_get0_group(ephermeal_keypair_server), server_e_pubkey_point,
                       POINT_CONVERSION_UNCOMPRESSED, server_e_pubkey, FHMQV_PUBKEY_UNCOMPRESSED_SIZE, NULL);

    uint8_t *server_e_privkey = malloc(server_e_privkey_len);
    BN_bn2bin(server_e_privkey_bn, server_e_privkey);


    EC_POINT *server_s_pubkey_point = EC_KEY_get0_public_key(static_keypair_server);
    BIGNUM *server_s_privkey_bn = EC_KEY_get0_private_key(static_keypair_server);
    size_t server_s_privkey_len = BN_num_bytes(server_s_privkey_bn);

    uint8_t server_s_pubkey[FHMQV_PUBKEY_UNCOMPRESSED_SIZE];
    EC_POINT_point2oct(EC_KEY_get0_group(static_keypair_server), server_s_pubkey_point,
                       POINT_CONVERSION_UNCOMPRESSED, server_s_pubkey, FHMQV_PUBKEY_UNCOMPRESSED_SIZE, NULL);

    uint8_t *server_s_privkey = malloc(server_s_privkey_len);
    BN_bn2bin(server_s_privkey_bn, server_s_privkey);


    /*********************/


    uint8_t sharedClient[48];
    uint8_t sharedServer[48];


    clock_t t = clock();
    int k;

    for (k = 0; k < 1000; k += 2) {
        fhmqv_calculate_shared_key(server_e_privkey, server_e_privkey_len,
                                   server_e_pubkey, FHMQV_PUBKEY_UNCOMPRESSED_SIZE,
                                   server_s_privkey, server_s_privkey_len,
                                   server_s_pubkey, FHMQV_PUBKEY_UNCOMPRESSED_SIZE,
                                   clnt_e_pubkey, FHMQV_PUBKEY_UNCOMPRESSED_SIZE,
                                   clnt_s_pubkey, FHMQV_PUBKEY_UNCOMPRESSED_SIZE,
                                   sharedServer, 48, 1);


        fhmqv_calculate_shared_key(clnt_e_privkey, clnt_e_privkey_len,
                                   clnt_e_pubkey, FHMQV_PUBKEY_UNCOMPRESSED_SIZE,
                                   clnt_s_privkey, clnt_s_privkey_len,
                                   clnt_s_pubkey, FHMQV_PUBKEY_UNCOMPRESSED_SIZE,
                                   server_e_pubkey, FHMQV_PUBKEY_UNCOMPRESSED_SIZE,
                                   server_s_pubkey, FHMQV_PUBKEY_UNCOMPRESSED_SIZE,
                                   sharedClient, 48, 0);

    }

    t = clock() - t;

    printf ("<TR><TD>%s<TD>%.2f<TD>%s<TD>\n", desc, ((((float)t)/CLOCKS_PER_SEC) / 1000) * 1000, comment);

    int i;
    for (i = 0; i < 48; i++) {
        if (sharedClient[i] != sharedServer[i]) {
            printf("error during calculation of the shared key\n");
        }
    }

    /*
    for (i = 0; i < 48; i++) {
        printf("%x:", sharedClient[i]);
    }
    printf("\n");
    printf("------------------------------------\n");
    for (i = 0; i < 48; i++) {
        printf("%x:", sharedServer[i]);
    }
    printf("\n");
    */

    return 0;

}


int test(int curve, char *desc)
{
    curve_id = curve;

    int k;

    for (k = 0; k < 700; k ++) {
        EC_KEY *static_keypair_server = EC_KEY_new_by_curve_name(curve_id);
        EC_KEY_generate_key(static_keypair_server);
        EC_KEY *ephermeal_keypair_server = EC_KEY_new_by_curve_name(curve_id);
        EC_KEY_generate_key(ephermeal_keypair_server);

        EC_KEY *static_keypair_client = EC_KEY_new_by_curve_name(curve_id);
        EC_KEY_generate_key(static_keypair_client);
        EC_KEY *ephermeal_keypair_client = EC_KEY_new_by_curve_name(curve_id);
        EC_KEY_generate_key(ephermeal_keypair_client);


        EC_POINT *clnt_e_pubkey_point = EC_KEY_get0_public_key(ephermeal_keypair_client);
        BIGNUM *clnt_e_privkey_bn = EC_KEY_get0_private_key(ephermeal_keypair_client);

        FHMQV_PUBKEY_UNCOMPRESSED_SIZE = EC_POINT_point2oct(EC_KEY_get0_group(ephermeal_keypair_client),
                                                            clnt_e_pubkey_point,
                                                            POINT_CONVERSION_UNCOMPRESSED, NULL, 0, NULL);
        size_t clnt_e_privkey_len = BN_num_bytes(clnt_e_privkey_bn);

        //printf("pubkey size: %u\n", FHMQV_PUBKEY_UNCOMPRESSED_SIZE);
        //printf("privkey size: %u\n", FHMQV_PRIVKEY_SIZE);


        uint8_t clnt_e_pubkey[FHMQV_PUBKEY_UNCOMPRESSED_SIZE];
        EC_POINT_point2oct(EC_KEY_get0_group(ephermeal_keypair_client), clnt_e_pubkey_point,
                           POINT_CONVERSION_UNCOMPRESSED, clnt_e_pubkey, FHMQV_PUBKEY_UNCOMPRESSED_SIZE, NULL);

        uint8_t *clnt_e_privkey = malloc(clnt_e_privkey_len);
        BN_bn2bin(clnt_e_privkey_bn, clnt_e_privkey);



        EC_POINT *clnt_s_pubkey_point = EC_KEY_get0_public_key(static_keypair_client);
        BIGNUM *clnt_s_privkey_bn = EC_KEY_get0_private_key(static_keypair_client);
        size_t clnt_s_privkey_len = BN_num_bytes(clnt_s_privkey_bn);

        uint8_t clnt_s_pubkey[FHMQV_PUBKEY_UNCOMPRESSED_SIZE];
        EC_POINT_point2oct(EC_KEY_get0_group(static_keypair_client), clnt_s_pubkey_point,
                           POINT_CONVERSION_UNCOMPRESSED, clnt_s_pubkey, FHMQV_PUBKEY_UNCOMPRESSED_SIZE, NULL);

        uint8_t *clnt_s_privkey = malloc(clnt_s_privkey_len);
        BN_bn2bin(clnt_s_privkey_bn, clnt_s_privkey);

        /*********************/

        EC_POINT *server_e_pubkey_point = EC_KEY_get0_public_key(ephermeal_keypair_server);
        BIGNUM *server_e_privkey_bn = EC_KEY_get0_private_key(ephermeal_keypair_server);
        size_t server_e_privkey_len = BN_num_bytes(server_e_privkey_bn);

        uint8_t server_e_pubkey[FHMQV_PUBKEY_UNCOMPRESSED_SIZE];
        EC_POINT_point2oct(EC_KEY_get0_group(ephermeal_keypair_server), server_e_pubkey_point,
                           POINT_CONVERSION_UNCOMPRESSED, server_e_pubkey, FHMQV_PUBKEY_UNCOMPRESSED_SIZE, NULL);

        uint8_t *server_e_privkey = malloc(server_e_privkey_len);
        BN_bn2bin(server_e_privkey_bn, server_e_privkey);


        EC_POINT *server_s_pubkey_point = EC_KEY_get0_public_key(static_keypair_server);
        BIGNUM *server_s_privkey_bn = EC_KEY_get0_private_key(static_keypair_server);
        size_t server_s_privkey_len = BN_num_bytes(server_s_privkey_bn);

        uint8_t server_s_pubkey[FHMQV_PUBKEY_UNCOMPRESSED_SIZE];
        EC_POINT_point2oct(EC_KEY_get0_group(static_keypair_server), server_s_pubkey_point,
                           POINT_CONVERSION_UNCOMPRESSED, server_s_pubkey, FHMQV_PUBKEY_UNCOMPRESSED_SIZE, NULL);

        uint8_t *server_s_privkey = malloc(server_s_privkey_len);
        BN_bn2bin(server_s_privkey_bn, server_s_privkey);


        /*********************/


        uint8_t sharedClient[48];
        uint8_t sharedServer[48];

        fhmqv_calculate_shared_key(server_e_privkey, server_e_privkey_len,
                                   server_e_pubkey, FHMQV_PUBKEY_UNCOMPRESSED_SIZE,
                                   server_s_privkey, server_s_privkey_len,
                                   server_s_pubkey, FHMQV_PUBKEY_UNCOMPRESSED_SIZE,
                                   clnt_e_pubkey, FHMQV_PUBKEY_UNCOMPRESSED_SIZE,
                                   clnt_s_pubkey, FHMQV_PUBKEY_UNCOMPRESSED_SIZE,
                                   sharedServer, 48, 1);


        fhmqv_calculate_shared_key(clnt_e_privkey, clnt_e_privkey_len,
                                   clnt_e_pubkey, FHMQV_PUBKEY_UNCOMPRESSED_SIZE,
                                   clnt_s_privkey, clnt_s_privkey_len,
                                   clnt_s_pubkey, FHMQV_PUBKEY_UNCOMPRESSED_SIZE,
                                   server_e_pubkey, FHMQV_PUBKEY_UNCOMPRESSED_SIZE,
                                   server_s_pubkey, FHMQV_PUBKEY_UNCOMPRESSED_SIZE,
                                   sharedClient, 48, 0);

        int i;
        for (i = 0; i < 48; i++) {
            if (sharedClient[i] != sharedServer[i]) {
                printf("(%d) error %s\n", k, desc);
                return;
            }
        }

    }

}

int main(int argc, char **argv)
{
    /*
    benchmark(NID_secp521r1, "NID_secp521r1: NIST/SECG curve over a 521 bit prime field (optimized)");

    //test(NID_secp256k1, "NID_secp256k1: SECG curve over a 256 bit prime field");
    //test(NID_secp384r1, "NID_secp384r1: NIST/SECG curve over a 384 bit prime field");

    benchmark(NID_secp192k1, "NID_secp192k1", "SECG curve over a 192 bit prime field");
    benchmark(NID_secp224k1, "NID_secp224k1", "SECG curve over a 224 bit prime field");
    benchmark(NID_secp224r1, "NID_secp224r1", "NIST/SECG curve over a 224 bit prime field (optimized)");
    benchmark(NID_secp256k1, "NID_secp256k1", "SECG curve over a 256 bit prime field");
    benchmark(NID_secp384r1, "NID_secp384r1", "NIST/SECG curve over a 384 bit prime field");
    benchmark(NID_secp521r1, "NID_secp521r1", "NIST/SECG curve over a 521 bit prime field (optimized)");



    // X9.62 curves
    benchmark(NID_X9_62_prime192v1, "NID_X9_62_prime192v1", "NIST/X9.62/SECG curve over a 192 bit prime field");
    benchmark(NID_X9_62_prime192v2, "NID_X9_62_prime192v2", " NIST/X9.62/SECG curve over a 192 bit prime field");
    benchmark(NID_X9_62_prime192v3, "NID_X9_62_prime192v3", " NIST/X9.62/SECG curve over a 192 bit prime field");

    benchmark(NID_X9_62_prime239v1, "NID_X9_62_prime239v1", " X9.62 curve over a 239 bit prime field");
    benchmark(NID_X9_62_prime239v2, "NID_X9_62_prime239v2", " X9.62 curve over a 239 bit prime field");
    benchmark(NID_X9_62_prime239v3, "NID_X9_62_prime239v3", " X9.62 curve over a 239 bit prime field");

    benchmark(NID_X9_62_prime256v1, "NID_X9_62_prime256v1", " X9.62/SECG curve over a 256 bit prime field (optimized)");
    */



    EC_KEY *keypair = EC_KEY_new_by_curve_name(NID_secp384r1);
    EC_KEY_generate_key(keypair);


    EC_POINT *pubkey_point = EC_KEY_get0_public_key(keypair);
    BIGNUM *privkey_bn = EC_KEY_get0_private_key(keypair);
    BIGNUM *dup = BN_new();
    BN_rshift(dup, privkey_bn, 80);

    size_t pubkey_len = EC_POINT_point2oct(EC_KEY_get0_group(keypair),
                                           pubkey_point,
                                           POINT_CONVERSION_UNCOMPRESSED, NULL, 0, NULL);
    size_t privkey_len = BN_num_bytes(privkey_bn);
    size_t dup_len = BN_num_bytes(dup);

    printf("pubkey size: %u\n", pubkey_len);
    printf("privkey size: %u\n", privkey_len);

    uint8_t *privkey_bin = malloc(privkey_len);
    BN_bn2bin(privkey_bn, privkey_bin);

    uint8_t *dup_bin = calloc(privkey_len, 1);
    BN_bn2bin(dup, dup_bin + 10);

    int i;
    for (i = 0; i < privkey_len; i++) {
        printf("%2x:", privkey_bin[i]);
    }
    printf("\n");

    for (i = 0; i < privkey_len; i++) {
        printf("%02x:", dup_bin[i]);
    }
    printf("\n");


    BIGNUM *bn1 = BN_new();
    BIGNUM *bn2 = BN_new();

    BN_bin2bn(dup_bin, privkey_len, bn1);
    BN_bin2bn(dup_bin + 11, dup_len, bn2);

    BN_print_fp(stdout, bn1);
    printf("\n");
    BN_print_fp(stdout, bn2);
}
