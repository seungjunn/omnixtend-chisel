/*
 * Raw Ethernet Packet Sender
 * EtherType: 0xAAAA (OmniXtend)
 */

#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <unistd.h>
#include <sys/socket.h>
#include <sys/ioctl.h>
#include <net/if.h>
#include <netinet/ether.h>
#include <arpa/inet.h>
#include <linux/if_packet.h>
#include <errno.h>

#define ETHER_TYPE 0xAAAA
#define BUF_SIZE 1518

// Ethernet header structure
struct eth_frame {
    unsigned char dest_mac[6];
    unsigned char src_mac[6];
    unsigned short ether_type;
    unsigned char payload[1500];
} __attribute__((packed));

void print_mac(const char* prefix, unsigned char* mac) {
    printf("%s%02x:%02x:%02x:%02x:%02x:%02x\n", 
           prefix, mac[0], mac[1], mac[2], mac[3], mac[4], mac[5]);
}

void print_hex_dump(const char* prefix, unsigned char* data, int len) {
    printf("%s", prefix);
    for (int i = 0; i < len; i++) {
        printf("%02x ", data[i]);
        if ((i + 1) % 16 == 0) printf("\n%*s", (int)strlen(prefix), "");
    }
    printf("\n");
}

int main(int argc, char *argv[]) {
    int sockfd;
    struct ifreq ifr;
    struct sockaddr_ll socket_address;
    struct eth_frame frame;
    int frame_len;
    
    // Default values
    char *ifname = "eth0";
    unsigned char dest_mac[6] = {0xff, 0xff, 0xff, 0xff, 0xff, 0xff}; // Broadcast
    char *payload_str = "Hello OmniXtend!";
    int payload_len;
    
    // Parse command line arguments
    if (argc >= 2) {
        ifname = argv[1];
    }
    if (argc >= 3) {
        // Parse destination MAC address (format: aa:bb:cc:dd:ee:ff)
        if (sscanf(argv[2], "%hhx:%hhx:%hhx:%hhx:%hhx:%hhx",
                   &dest_mac[0], &dest_mac[1], &dest_mac[2],
                   &dest_mac[3], &dest_mac[4], &dest_mac[5]) != 6) {
            fprintf(stderr, "Invalid MAC address format. Use: aa:bb:cc:dd:ee:ff\n");
            return 1;
        }
    }
    if (argc >= 4) {
        payload_str = argv[3];
    }
    
    printf("=== Raw Ethernet Packet Sender ===\n");
    printf("Interface: %s\n", ifname);
    print_mac("Destination MAC: ", dest_mac);
    printf("EtherType: 0x%04x\n", ETHER_TYPE);
    printf("Payload: %s\n", payload_str);
    printf("==================================\n\n");
    
    // Create raw socket
    sockfd = socket(AF_PACKET, SOCK_RAW, htons(ETH_P_ALL));
    if (sockfd < 0) {
        perror("socket() failed");
        printf("Note: You need root privileges to create raw sockets.\n");
        printf("Try: sudo %s %s\n", argv[0], ifname);
        return 1;
    }
    
    // Get interface index
    memset(&ifr, 0, sizeof(ifr));
    strncpy(ifr.ifr_name, ifname, IFNAMSIZ - 1);
    if (ioctl(sockfd, SIOCGIFINDEX, &ifr) < 0) {
        perror("ioctl(SIOCGIFINDEX) failed");
        close(sockfd);
        return 1;
    }
    
    int ifindex = ifr.ifr_ifindex;
    
    // Get source MAC address
    if (ioctl(sockfd, SIOCGIFHWADDR, &ifr) < 0) {
        perror("ioctl(SIOCGIFHWADDR) failed");
        close(sockfd);
        return 1;
    }
    
    unsigned char *src_mac = (unsigned char *)ifr.ifr_hwaddr.sa_data;
    print_mac("Source MAC: ", src_mac);
    printf("\n");
    
    // Build Ethernet frame
    memset(&frame, 0, sizeof(frame));
    
    // Destination MAC
    memcpy(frame.dest_mac, dest_mac, 6);
    
    // Source MAC
    memcpy(frame.src_mac, src_mac, 6);
    
    // EtherType (0xAAAA in network byte order)
    frame.ether_type = htons(ETHER_TYPE);
    
    // Payload
    payload_len = strlen(payload_str);
    if (payload_len > sizeof(frame.payload)) {
        payload_len = sizeof(frame.payload);
    }
    memcpy(frame.payload, payload_str, payload_len);
    
    // Minimum Ethernet frame size (without FCS) is 60 bytes
    // Header is 14 bytes, so minimum payload is 46 bytes
    int min_payload = 46;
    if (payload_len < min_payload) {
        // Pad with zeros
        memset(frame.payload + payload_len, 0, min_payload - payload_len);
        payload_len = min_payload;
    }
    
    frame_len = 14 + payload_len; // 14 bytes header + payload
    
    // Print frame details
    printf("Frame details:\n");
    printf("  Frame length: %d bytes\n", frame_len);
    print_hex_dump("  Frame data: ", (unsigned char *)&frame, frame_len);
    printf("\n");
    
    // Setup socket address structure
    memset(&socket_address, 0, sizeof(socket_address));
    socket_address.sll_ifindex = ifindex;
    socket_address.sll_halen = ETH_ALEN;
    memcpy(socket_address.sll_addr, dest_mac, 6);
    
    // Send the frame
    printf("Sending frame...\n");
    int sent_bytes = sendto(sockfd, &frame, frame_len, 0,
                           (struct sockaddr *)&socket_address,
                           sizeof(socket_address));
    
    if (sent_bytes < 0) {
        perror("sendto() failed");
        close(sockfd);
        return 1;
    }
    
    printf("Successfully sent %d bytes!\n", sent_bytes);
    
    close(sockfd);
    return 0;
}

