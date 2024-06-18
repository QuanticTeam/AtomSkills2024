import { apiClient } from '~/shared/apiClient'
import { UserRoleEnum } from '~/shared/auth'

export interface UserRole {
  id: UserRoleEnum
  role: keyof typeof UserRoleEnum
}

export const UserRoleApi = {
  async fetchUserRoles() {
    return (await apiClient.get('/User/GetRoles')).data
  },
}
