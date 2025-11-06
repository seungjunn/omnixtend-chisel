# Raw Ethernet Packet Sender

OmniXtend용 Raw 이더넷 패킷 전송 유틸리티입니다.

## 빌드

```bash
cd util
make
```

## 사용법

```bash
sudo ./send_ethernet [interface] [dest_mac] [payload]
```

### 파라미터

- `interface`: 네트워크 인터페이스 (기본값: eth0)
- `dest_mac`: 목적지 MAC 주소 (기본값: ff:ff:ff:ff:ff:ff - broadcast)
- `payload`: 전송할 데이터 (기본값: "Hello OmniXtend!")

### 예제

1. **기본 전송 (broadcast)**
```bash
sudo ./send_ethernet eth0
```

2. **특정 MAC 주소로 전송**
```bash
sudo ./send_ethernet eth0 00:11:22:33:44:55
```

3. **커스텀 페이로드 전송**
```bash
sudo ./send_ethernet eth0 00:11:22:33:44:55 "Custom OmniXtend Message"
```

4. **다른 인터페이스 사용**
```bash
sudo ./send_ethernet enp0s3 ff:ff:ff:ff:ff:ff "Test Message"
```

## EtherType

이 프로그램은 EtherType을 **0xAAAA**로 설정하여 OmniXtend 프로토콜 패킷으로 인식됩니다.

## 네트워크 인터페이스 확인

사용 가능한 네트워크 인터페이스 목록:

```bash
ip link show
# 또는
ifconfig -a
```

## 참고사항

- Raw socket을 생성하려면 **root 권한**이 필요합니다 (sudo 사용)
- 최소 프레임 크기는 60바이트(헤더 제외)입니다
- 페이로드가 46바이트 미만이면 자동으로 0으로 패딩됩니다

## 패킷 캡처

전송된 패킷을 확인하려면:

```bash
# 터미널 1: 패킷 캡처
sudo tcpdump -i eth0 -XX ether proto 0xaaaa

# 터미널 2: 패킷 전송
sudo ./send_ethernet eth0
```

또는 Wireshark를 사용:
```bash
sudo wireshark -i eth0 -f "ether proto 0xaaaa"
```

