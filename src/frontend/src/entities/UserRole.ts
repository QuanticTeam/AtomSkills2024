import { apiClient } from '~/shared/apiClient'

export interface UserRole {
  id: number
  role: string
}

export const UserRoleApi = {
  async fetchUserRoles() {
    return (await apiClient.get('/User/GetRoles')).data
  },
}
