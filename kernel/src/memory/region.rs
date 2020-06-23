use super::phys::PhysicalMemory;
use super::VirtualAddress;

pub struct VirtualRegion {
    start: VirtualAddress,
    size: usize,
    backing: Option<PhysicalMemory>,
}

impl VirtualRegion {
    pub fn start(&self) -> VirtualAddress {
        self.start
    }

    pub fn size(&self) -> usize {
        self.size
    }

    pub fn end(&self) -> VirtualAddress {
        self.start + self.size
    }

    pub fn cleave(self, size: usize) -> Result<(VirtualRegion, VirtualRegion), VirtualRegion> {
        if self.backing.is_some() || size > self.size - 8 || self.size % 8 != 0 {
            Err(self)
        } else {
            Ok((
                VirtualRegion {
                    start: self.start,
                    size,
                    backing: None,
                },
                VirtualRegion {
                    start: self.end(),
                    size: self.size - size,
                    backing: None,
                },
            ))
        }
    }

    pub fn coalesce(
        self,
        other: VirtualRegion,
    ) -> Result<VirtualRegion, (VirtualRegion, VirtualRegion)> {
        if self.backing.is_some() || other.backing.is_some() {
            Err((self, other))
        } else if other.start < self.start {
            other.coalesce(self).map_err(|(o, s)| (s, o))
        } else if self.end() != other.start {
            Err((self, other))
        } else {
            Ok(VirtualRegion {
                start: self.start,
                size: self.size + other.size,
                backing: None,
            })
        }
    }
}
